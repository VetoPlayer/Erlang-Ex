-module(what_the_if).
-compile(export_all). %%In production code this should be avoided



heh_fine() ->
    if 1 =:= 1 ->
        works
    end,
        if 1 =:= 2; 1 =:= 1 ->
        works
    end,
    if 1 =:= 2, 1 =:= 1 ->
        fails
    end.