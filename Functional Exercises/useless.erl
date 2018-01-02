-module(useless).
-export([add/2,hello/0,hello_and_add_two/1]).

add(A,B) ->
    A + B.


hello() ->
    io:format("Hello World!").

%%Shows greetings
hello_and_add_two(X) ->
    hello(),
    add(X,2).