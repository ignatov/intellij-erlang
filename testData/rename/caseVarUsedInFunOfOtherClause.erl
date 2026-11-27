-module(caseVarUsedInFunOfOtherClause).
-export([f/1]).
f(X) ->
    case X of
        1 -> Y = 1, _ = fun() -> Y end;
        _ -> <caret>Y = 2
    end.
