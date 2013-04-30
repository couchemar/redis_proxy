
-module(redis_proxy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Opts), {I, {I, start_link, Opts}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Sentinels} = application:get_env(sentinels),
    {ok, {
       {one_for_one, 5, 10},
       [?CHILD(redis_conn_sup, supervisor, []),
        ?CHILD(eredis_sentinel, worker, [Sentinels])]
      }
    }.

