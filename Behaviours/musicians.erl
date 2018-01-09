-module(musicians).
-behaviour(gen_server).

-module(musicians).
-behaviour(gen_server).
 
-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
handle_info/2, code_change/3, terminate/2]).
 
-record(state, {name="", role, skill=good}).
-define(DELAY, 750).
 
start_link(Role, Skill) ->
gen_server:start_link({local, Role}, ?MODULE, [Role, Skill], []).
 
stop(Role) -> gen_server:call(Role, stop).