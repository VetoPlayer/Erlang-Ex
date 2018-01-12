-module(erlcount).
-behaviour(application).

-export([start/2, stop/1]).

start(normal, Args) ->
    erlcount_sup:start_link().

stop(_State) ->
    ok.