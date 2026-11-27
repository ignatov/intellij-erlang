-module(comprehensionVarShadowsOtherCaseClause).
-export([f/2]).
f(K, L) ->
    case K of
        1 -> [K || <caret>K <- L];
        _ -> K
    end.
