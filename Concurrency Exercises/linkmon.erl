-module(linkmon).
-compile(export_all).

start_critic() ->
    spawn(?MODULE,critic,[]).

%% Function to interact with critic hiding the protocol specifications.
judge(Band, Album) ->
    critic ! {self(),{Band, Album}},
    Pid = whereis(critic),
    receive
        {CriticPid, Criticism} -> Criticism
    after 2000 ->
        timeout
    end.

%% Restarted spawns and links to the critic modules.
%% When critic exits for some reason, it flags itself to not die but to restart the process. 
restarter() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE,critic,[]),
    register(critic,Pid),
    receive
        {'EXIT',pid, normal} ->
            ok;
        {'EXIT',pid, shutdown} ->
            ok;
        {'EXIT',pid, _} ->
            restarter()
    end.

critic() ->
    receive
        {From, {"Oishin", "DIIV"}} ->
            From ! {self(),"That's great"};
        {From, {"Nasty", "Nastyness"}} ->
            From ! {self(),"That's great!"};
        {From, {_Band, _Album}} ->
            From ! {self(),"Really bad"}
    end,
    critic().

