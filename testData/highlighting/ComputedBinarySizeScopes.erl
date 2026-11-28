-module('ComputedBinarySizeScopes').
-export([earlier/1, before_match/2, generator/2, case_pattern/2, fun_head/1, nested/1, comprehension/1,
         unbound_generator/1, unbound_case/1, unbound_fun/0]).

earlier(<<Len:8, Data:(Len*8)/bits>>) -> Data.
before_match(N, B) -> <<X:(N*8)>> = B, X.
generator(N, L) -> [X || <<X:(N*8)>> <- L].
case_pattern(N, B) -> case B of <<X:(N*8)>> -> X end.
fun_head(N) -> fun(<<X:(N*8)>>) -> X end.
nested(B) -> <<A:8, X:(byte_size(<<0:A>>))>> = B, X.
comprehension(L) -> <<0:(lists:sum([S || {_, S} <- L]))>>.
unbound_generator(L) -> [X || <<X:(<error descr="Variable 'Y' is unbound">Y</error>*8)>> <- L].
unbound_case(B) -> case B of <<X:(<error descr="Variable 'M' is unbound">M</error>*8)>> -> X end.
unbound_fun() -> fun(<<X:(<error descr="Variable 'K' is unbound">K</error>*8)>>) -> X end.
