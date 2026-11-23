-export([a/1, b/1, c/1, d/1, e/1, f/1, h/2, i/2, j/1]).

a(<<L:8, (<error>L+1</error>):8>>) -> L.
b(#{k := <error>X+1</error>}) -> X.
c(<<X:(<error>case 1 of _ -> 8 end</error>)>>) -> X.
d(<<X:(length(<error>"a" ++ "b"</error>))>>) -> X.
e(<<X:(<error>fun() -> 8 end</error>)>>) -> X.
f(<error>X+1</error>) -> X.
h(M, K) -> #{K =:= 1 := V} = M, V.
i(L, B) -> <<X:(erlang:length(L)), Y:(L andalso true)>> = B, {X, Y}.
j(M) -> K = [1], #{hd(K) + 1 := V} = M, V.
