-module('MapConstructionMatchOperatorContexts').
-export([ok/1, ok2/1, ok3/1, ok4/1, ok5/1, ok6/1, ok7/1, ok8/1, ok9/1, ok10/1, ok11/1, f/1, g/1, h/1]).
-define(P, #{a := X}).
-define(ID(E), E).

ok(M) -> M#{a := 1}.
ok2(#{a := X}) -> X.
ok3(M) -> #{a := X} = M, [Y] = [#{b := Y}] = [M], {X, Y}.
ok4(L) -> [V || #{a := V} <- L].
ok5(M) -> (M)#{a := 1}#{b := 2}.
ok6(M) -> case M of #{a := #{b := X}} when X > 0 -> X; _ -> receive #{c := Y} -> Y end end.
ok7(M) -> try M catch error:#{a := X} -> X end.
ok8(M) -> F = fun(#{a := X}) -> X end, F(M).
ok9(M) -> ?P = M, X.
ok10(M) -> [K || K := _ <- M].
ok11(M) -> ?ID(M#{a := 1}).

f(X) when X =:= #{a <error descr="only association operators '=>' are allowed in map construction">:=</error> 1} -> ok.
g(M) -> X = #{a <error descr="only association operators '=>' are allowed in map construction">:=</error> M}, X.
h(M) -> fun() -> #{a <error descr="only association operators '=>' are allowed in map construction">:=</error> M} end.
