-module(dolphins).
-compile(export_all).

%%Building up processes that receive a message

dolphin1() ->
    receive
        do_a_flip ->
            io:format("I don't feel like to do a flip");
        fish ->
            io:format("Thank you, I love fish");
        _ ->
            io:format("I am smarter than you but I can't understand what you mean")
    end.

%% Let's build up a dolphin which replies with a message to the sender
dolphinReply() ->
    receive
        {From,do_a_flip} ->
            From ! "I don't feel like to do a flip";
        {From,fish} ->
            From ! "Thank you, I love fish";
        {From,_} ->
            From ! "I am smarter than you but I can't understand what you mean"
    end.

%% With recursion the dolphin call itself and can consume potentially infinite messages.
%% The stack doesn't blow up because of tail-recursion.

dolphinFinal() ->
    receive
        {From,do_a_flip} ->
            From ! "I don't feel like to do a flip".
            dolphinFinal();
        {From,fish} ->
            From ! "Thank you, I love fish",
            dolphinFinal();
        {From,_} ->
            From ! "I am smarter than you but I can't understand what you mean",
            dolphinFinal()
    end.