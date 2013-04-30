-module(redis_proxy_protocol).
-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API
-export([start_link/4, send/2]).

%% gen_server callbacks
-export([init/4, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket, transport, linked_conn}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

send(Pid, Data) ->
    gen_server:call(Pid, {to_client, Data}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Ref, Socket, Transport, _Opts) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    {ok, RedisConnectPid} = redis_conn_sup:start_conn(self()),
    ok = Transport:setopts(Socket, [{active, once}]),
    gen_server:enter_loop(
      ?MODULE, [],
      #state{socket=Socket, transport=Transport,
             linked_conn=RedisConnectPid}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({to_client, Data}, _From, #state{socket=Socket, transport=Transport} = State) ->
    lager:info("Sending ~p, to clent", [Data]),
    Transport:send(Socket, Data),
    Reply = ok,
    {reply, Reply, State};
handle_call(Request, _From, State) ->
    lager:info("Unhandled call ~p", [Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    lager:info("Unhandled cast ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, _Socket, Data}, #state{linked_conn = RedisPid} = State) ->
    lager:info("Got: ~p~n", [Data]),
    inet:setopts(State#state.socket, [{active, once}]),
    redis_conn:send(RedisPid, Data),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    lager:info("Connection closed"),
    {stop, normal, State};
handle_info(Info, State) ->
    lager:info("Unhandled info ~p ~n", [Info]),
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    lager:debug("Terminate cause ~p~n", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
