-module(functions).
-compile(export_all). %%In production code this should be avoided.

%% Head of a list
head([H|_]) ->
    H.

%% Second element of a list
second([_,X | _]) ->
    X.

tail_duplicate(0,_,Accum) ->
    Accum;
tail_duplicate(N,Term,Accum) -> tail_duplicate(N-1, Term, [Term | Accum]).

tail_duplicate(N,Term) ->
         tail_duplicate(N, Term, []).

%% Reverse a List

tail_reverse([]) ->
    [];
tail_reverse(List) ->
    tail_reverse(List,[]).

tail_reverse([], Accum) ->
        Accum;
tail_reverse([H|T], Accum) ->
    tail_reverse(T,[H | Accum]).

tail_sublist(N,List)->
    tail_sublist(N,List,[]).

tail_sublist(0,_,Accum) ->
    Accum;
tail_sublist(N,[H|T],Accum) ->
    tail_sublist(N-1,T,[Accum | H]).