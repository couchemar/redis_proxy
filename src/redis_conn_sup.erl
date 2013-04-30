-module(redis_conn_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_conn/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_conn(ProxyPid) ->
    supervisor:start_child(?MODULE, [ProxyPid]).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {redis_conn, {redis_conn, start_link, []},
              Restart, Shutdown, Type, [redis_conn]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

