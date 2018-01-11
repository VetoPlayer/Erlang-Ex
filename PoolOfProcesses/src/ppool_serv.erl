-module(ppool_serv).
-behaviour(gen_server).
-export().

%% ----- External API --------

%% It starts the server
start(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
    gen_server:start({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

%% Starts and link the server
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


%% The friendly supervisor is started dynamically
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



%% In order to dequeue something we have monitors everywhere: let's use them
handle_info({'DOWN', Ref, process, _Pid, _}, S=#state{refs=Refs}) ->
    io:format("Received down message"),
    case gb_sets:is_element(Ref,Refs) of
        true ->
            handle_down_worker(Ref,S);
        false ->
            {noreply, S}    
    end

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


handle_call({run, Args}, _From, S=#state{limit=N, sup = Sup, refs=R})  when N > 0 ->
    %% There is place in the pool left
    {ok, Pid} -> supervisor:start_child(Sup,Args),
    Ref = erlang:monitor(process, Pid),
    {noreply, S#state{limit=N-1, refs=gb_sets:add(Ref,R)}};
handle_call({run, _Args}, _From, S=#state{limit=N}) when N =< 0 ->
    {reply, noalloc, S};

handle_call({sync, Args}, _From, S=#state{limit=N, sup = Sup, refs=R}) when N > 0 ->
    %% There is place in the pool left
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {noreply, S#state{limit=N-1, refs=gb_sets:add(Ref,R)}};
handle_call({run, _Args}, From, S=#state{queue=Q}) when N =< 0 ->
    %% No place in the pool: we add it to the queue
    {noreply,S#state{queue=queue:in({From,Args},Q)}};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({async, Args},_From, S=#state{sup=Sup,limit=N, refs=R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup,Args),
    Ref = erlang:monitor(process,Pid),
    {noreply, S#state{limit=N-1, refs=gb_sets:add(Ref,R)}};
handle_cast({async, Args},From, S=#state{queue=Q}) when N =< 0 ->
    {noreply, S#state{queue=queue:in({From,Args},Q)}};
handle_cast(_Msg, S) ->
    {noreply,S}.


handle_down_worker(Ref, S=#state{limit=L, sup=S, refs=Refs}) ->
    case queue:out(S#state.queue) of
        {{value,{From, Args}}, Q} -> %% Synchronous call
            {ok,Pid} -> supervisor:start_child(Sup,Args),
            NewRef = erlang:monitor(process, Pid),
            NewRefs = gb_sets:insert(NewRef,gb_sets:delete(Ref, Refs)),
            gen_server:reply(From, {ok, Pid}),
            {noreply, S#state{refs=NewRefs,queue=Q}};
        {{value, Args}, Q} -> %% Asynchronous call
            {ok, Pid} = supervisor:start_child(Sup,Args),
            NewRef = erlang:monitor(process,Pid),
            NewRefs = gb_insert(NewRef,gb_sets:delete(Ref,Refs));
        {empty,_} ->
            {noreply, S#state{limit=L+1,refs=gb_sets:delete(Ref,Refs)}} 
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
    

    


