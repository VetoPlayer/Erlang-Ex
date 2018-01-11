-module(ppool_supersup).

-behaviour(supervisor).
%% Starts the whole server, stops it, starts ands stops a specific pool.
-export([start_link/0, start_pool/3, stop_pool/1]).
-export([init/1]).

%% Starts the super-supervisor and gives it the name of ppool.
start_link() ->
    supervisor:start_link({local,ppool},?MODULE,[]).


init([]) ->
    MaxRestart = 6,
    MaxTime = 3600,
    {ok, {one_for_one, MaxRestart, MaxTime},[]}.

start_pool(Name, Limit, MFA) ->
    ChildSpec = {Name,
                {ppool_sup, start_link,[Name, Limit, MFA]},
                permanent, 10500, supervisor,[ppool_sup]},
    supervisor:start_child(ppool, ChildSpec).


%% This is possible because we gave the child a Name in the child specifier
stop_pool(Name) ->
    supervisor:terminate_child(ppool, Name),
    supervisor:delete_child(ppool, Name).





