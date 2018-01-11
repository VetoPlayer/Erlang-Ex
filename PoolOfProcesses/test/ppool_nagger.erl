-module(ppool_nagger).
-behaviour(gen_server).

%% API
-export([start/1, stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {dummy}).

start([]) ->
    ok.

stop(Pid) ->
   gen_server:call(Pid, stop).

start_link({Task,Delay,Max,SendTo}) ->
   gen_server:start_link(?MODULE, {Task,Delay,Max,SendTo}, []).

init({Task, Delay, Max, SendTo}) ->
   {ok, {Task, Delay,Max, SendTo}, Delay}.

%% Task : Thing to send as a message
%% Delay : Time spent between each sending
%% Max : Maximum number of messsages to send
%% SendTo: Pid or Name to sent the message

handle_call(stop, _From, State) ->
   {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(timeout, {Task, Delay, Max, SendTo}) ->
    SendTo ! {self(), Task},
    if Max =:= infinity ->
        {noreply, {Task, Delay, Max, SendTo}, Delay};
        Max =< 1 ->
            {stop, normal,{Task, Delay,Max, SendTo}};
        Max > 1 ->
            {noreply, {Task, Delay, Max-1, SendTo}, Delay}  
    end.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.





