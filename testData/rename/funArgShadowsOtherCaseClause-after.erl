-module(funArgShadowsOtherCaseClause).
-export([f/1]).
f(K) ->
    case K of
        1 -> fun(<caret>New) -> ok end;
        _ -> K
    end.
