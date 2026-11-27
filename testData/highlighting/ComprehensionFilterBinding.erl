-export([r1/1, r2/1, r3/1, r5/1, r6/1, r8/1, r9/1, r11/1]).

r1(L) -> [X || A <- L, begin X = A, true end].

r2(Keys) -> [V || K <- Keys, (V = get(K)) =/= undefined].

r3(L) -> [A || A <- L, (X = A) > 1, X > 2].

r5(L) -> #{K => V || K <- L, (V = K) =/= 1}.

r6(L) -> << <<V>> || <<K>> <= L, (V = K) =/= 1 >>.

r8(L) -> [Y || A <- L, is_integer(Y = A)].

r9(L) -> [begin Z = Y, Z end || A <- L, (Y = A) > 0].

r11(L) -> [V || {ok, V0} <- L, (V = V0) =/= undefined].
