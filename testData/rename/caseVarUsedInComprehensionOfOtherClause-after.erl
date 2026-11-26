-module(caseVarUsedInComprehensionOfOtherClause).
-export([f/1]).
f(X) ->
    case X of
        1 -> New = 1, _ = [New || _ <- [a]];
        _ -> <caret>New = 2
    end.
