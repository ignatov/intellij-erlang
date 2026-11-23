-export([a/1, b/1, c/1, d/1, e/1, f/1, h/2, i/2, j/1, fa/2, fb/2, fc/2, k2/2, cmp/1, op/2, foo/1]).

a(<<L:8, (<error>L+1</error>):8>>) -> L.
b(#{k := <error>X+1</error>}) -> X.
c(<<X:(<error>case 1 of _ -> 8 end</error>)>>) -> X.
d(<<X:(length(<error>"a" ++ "b"</error>))>>) -> X.
e(<<X:(<error>fun() -> 8 end</error>)>>) -> X.
f(<error>X+1</error>) -> X.
h(M, K) -> #{K =:= 1 := V} = M, V.
i(L, B) -> <<X:(erlang:length(L)), Y:(L andalso true)>> = B, {X, Y}.
j(M) -> K = [1], #{hd(K) + 1 := V} = M, V.
foo(X) -> X.
fa(L, B) -> <<X:(<error>foo(L)</error>)>> = B, X.
fb(L, B) -> <<X:(lists:<error>sum(L)</error>)>> = B, X.
fc(M, K) -> #{<error>foo(K)</error> := V} = M, V.
k2(M, K) -> #{<error>K+1</error> => V} = M, V.
cmp(<error>X == 1</error>) -> X.
op(L, B) -> <<X:(erlang:'+'(L, 1))>> = B, X.
