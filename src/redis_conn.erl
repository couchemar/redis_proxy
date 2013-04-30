-module(redis_conn).

-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

%% API
-export([start_link/1, send/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {host, port, cluster, socket, proxy_pid}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ProxyPid) ->
    gen_server:start_link(?MODULE, [ProxyPid], []).

send(Pid, Data) ->
    ok = gen_server:call(Pid, {to_redis, Data}, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ProxyPid]) ->
    _Ref = monitor(process, ProxyPid),
    State = #state{cluster=cache, proxy_pid=ProxyPid},
    connect(State).

handle_call({to_redis, Data}, _From, #state{host = Host, port = Port, socket = Socket} = State) ->
    lager:debug("Send ~p to ~p:~p", [Data, Host, Port]),
    ok = gen_tcp:send(Socket, Data),
   {reply, ok, State};
handle_call(Request, _From, State) ->
    lager:debug("Unhandled request ~p~n", [Request]),
    {stop, {unknown_request, Request}, State}.

handle_cast(Msg, State) ->
    lager:debug("Unhandled msg ~p~n", [Msg]),
    {noreply, State}.

handle_info({tcp, _Socket, Data}, #state{proxy_pid = ProxyPid} = State) ->
    lager:debug("Got ~p from redis~n", [Data]),
    inet:setopts(State#state.socket, [{active, once}]),
    redis_proxy_protocol:send(ProxyPid, Data),
    {noreply, State};
handle_info({tcp_closed, _Socket}, #state{proxy_pid = ProxyPid}= State) ->
    lager:info("Connection to redis closed, wait for sentinels"),
    {ok, State1} = connect(State),
    {noreply, State1};
handle_info({'DOWN', _Ref, process, _Pid, _Info}, State) ->
    lager:info("Die because proxy connection down"),
    supervisor:terminate_child(redis_conn_sup, self()),
    {noreply, State};
handle_info(Info, State) ->
    lager:debug("Unhandled msg ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect(#state{cluster=Cluster} = State) ->
    {ok, {Host, Port}} = eredis_sentinel:get_master(Cluster),
    lager:debug("Opening connection to redis to ~p:~p", [Host, Port]),

    case  gen_tcp:connect(
            Host, Port,
            [binary, {active, once}, {packet, raw}, {reuseaddr, true}]
           ) of
        {ok, Socket} ->
            lager:debug("Connection to redis to ~p:~p opened", [Host, Port]),
            {ok, State#state{socket = Socket, host = Host, port = Port}};
        {error, Error} ->
            lager:error("Could not open connection to ~p:~p, Error: ~p~n",
                        [Host, Port, Error]),
            timer:sleep(1000),
            connect(State)
    end.
