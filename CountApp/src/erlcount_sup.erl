-module(erlcount_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init() ->
    MaxRestart = 5,
    MaxTime=100,
    Shutdown = 60000,
    {ok, {{one_for_one, MaxRestart, MaxTime},
    [dispatch, {erlcount_dispatch, start_link,[]},
    transient, Shutdown, worker, [erlcount_dispatch]}]}}.