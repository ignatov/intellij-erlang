-module(caseVarUsedInComprehensionOfOtherClause).
-export([f/1]).
f(X) ->
    case X of
        1 -> Y = 1, _ = [Y || _ <- [a]];
        _ -> <caret>Y = 2
    end.
