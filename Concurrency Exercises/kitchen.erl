-module(kitchen).
-compile(export_all).

%% Implementing actors just as functions with messages isn't enough.
%% We want to be able to hold a state in the process.

%% Let's implemente a fridge which stores food.
%% We implement this with the help of recursion: we give a list of foods as input to the fridge 

fridge(FoodList) ->
    receive
        {From, {store, Food}} ->
            From ! {self(), ok}, %% Sends the ok message to the sender
            fridge([Food | FoodList]);
        {From, {take, Food}} ->
            case lists:member(Food, FoodList) of
                true ->
                    From ! {self(), {ok,Food}},
                    fridge(lists:delete(Food,FoodList));
                false ->
                    From ! {self(), not_found},
                    fridge(FoodList)
            end;
        terminate ->
            ok;
        Unexpected ->
            io:format("unexpected message ~p~n", [Unexpected])
    end.

%% Let's define a standard way to interact with a fridge from the shell without forcing the shell to know how
%% the message is passed

%% We define a standard wrapper function for each message corresponding to an exposed functionality:

store(FridgeId, Item) -> %% Hide the protocol for message passing
    FridgeId ! {self(),{store,Item}}, %% Send a message to the fridge
    receive
        {FridgeId,Msg} -> Msg %% Waits to receive a response and return it.
    after 3000 ->
        timeout
    end.

take(FridgeId,Item) ->
    FridgeId ! {self(), {take, Item}},
    receive
        {FridgeId,Msg} -> Msg
    after 3000 ->
        timeout
    end.

%% We create a standard way to create the fridge too:

start(FoodList) ->
    spawn(?MODULE, fridge, [FoodList]).

%% Everything about the fridge process is now handled in the kitchen.erl module

