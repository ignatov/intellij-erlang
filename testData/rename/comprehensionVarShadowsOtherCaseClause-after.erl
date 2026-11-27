-module(comprehensionVarShadowsOtherCaseClause).
-export([f/2]).
f(K, L) ->
    case K of
        1 -> [New || <caret>New <- L];
        _ -> K
    end.
