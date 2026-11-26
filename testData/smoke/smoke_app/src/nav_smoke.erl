-module(nav_smoke).
-export([named/1, case_head/1]).

named(L) ->
    F = fun Loop([]) -> 0;
            Loop([_ | T]) -> 1 + Loop(T)
        end,
    F(L).

case_head(X) ->
    case Y = X + 1 of
        2 -> Y;
        _ -> {other, Y}
    end.
