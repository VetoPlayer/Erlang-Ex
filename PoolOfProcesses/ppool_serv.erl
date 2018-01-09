-module(ppool_serv).
-behaviour(gen_server).
-export().

%% ----- External API --------

%% Don't kwow - It should start the server
start(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
    gen_server:start({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

start_link(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
    gen_server:start_link({local, Name}, ?MODULE,{Limit,MFA,Sup}, []).

run(Name, Args) ->
    gen_server:call(Name,{run, Args}).

sync_queue(Name, Args) ->
    gen_server:call(Name, {sync, Args}, infinity).

async_queue(Name, Args) ->
    gen_server:cast(Name,{async, Args}).

stop(Name) ->
    gen_server:call(Name, stop).

%% ----- Internals -------


%% The frindly supervisor is started dynamically
-define(SPEC(MFA),
        {worker_sup,
        {ppool_worker_sup, start_link, [MFA]},
        temporary,
        10000,
        supervisor,
        [ppool_worker_sup]}).

%% Inner state of the server.
%% We want to keep track of: -> Number of processes that can be running together
%%                            -> pid of the supervisor
%%                            -> queue of all the jobs

%% To know when a worker's done running to fetch one from the queue to start it,
%% we need to track each worker from the server.
%%  The sane way to do this is to add monitors:
%% We add refs to the state to keep track of all the monitor references.

-record(state, {
    limit,      %% Number of processes that can be running together
    sup, %% Supervisor pid
    refs, %% Keeps track of monitor references
    queue= queue:new() %% By defaults initialize a new, empty queue of jobs.
}).

init({Limit, MFA, Sup}) ->
    %% We need to find the Pid of the worker supervisor from here,
    %% but alas, this would be calling the supervisor while it waits for us!
    %% We send a special message to ourself:
    self() ! {start_woker_supervisor, Sup, MFA}
    {ok, #state{limit=Limit, refs=gb_sets:empty()}}.


%% Handle_info takes care of handling spacial messages.
%% Here it handles a special message dealing with the worker supervisor initialization.
%% We send this message from the server to itself when the init() function is called.

handle_info({start_woker_supervisor, Sup, MFA}) ->
    {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),
    link(Pid),
    {noreply, S#state{sup=Pid}};
handle_info(Msg, State) ->
    io:format("Unknown message ~p~n", [Msg]),
    {noreply, State}.


    


