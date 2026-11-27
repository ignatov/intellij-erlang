-export([f/2, g/2]).

f(X, F) ->
    case F() of
        X -> X = F()
    end.

g(X, Y) -> case Y of _ -> X = Y, X = Y end.
