-module(aaa).

-spec                   f/0 ::            ()           -> atom().
f() ->
    case                                   f() of
        x -> x
    end.

f(X)                               when              is_atom(X) -> x.

f() ->
    "x" ++
        "y" ++
            "z".