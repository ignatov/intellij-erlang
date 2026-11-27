-module(caseVarUsedInFunOfLaterClause).
-export([f/1]).
f(X) ->
    case X of
        1 -> <caret>Y = 1;
        _ -> Y = 2, _ = fun() -> Y end
    end.
