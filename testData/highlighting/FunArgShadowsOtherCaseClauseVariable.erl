-module('FunArgShadowsOtherCaseClauseVariable').
-export([f/1]).
f(K) ->
    case K of
        1 -> fun(<warning descr="Unused variable 'K'">K</warning>) -> ok end;
        _ -> K
    end.
