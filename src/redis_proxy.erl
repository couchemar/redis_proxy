-module(redis_proxy).

-export([start/0]).

start() ->
    ok = sync:go(),
    ok = application:start(lager),
    ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(redis_proxy).
