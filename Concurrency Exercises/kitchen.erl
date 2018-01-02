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
            ok
    end.
