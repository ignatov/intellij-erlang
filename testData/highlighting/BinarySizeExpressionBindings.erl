-export([c1/1, c2/1, c3/1, c4/2, p22/1]).

c1(Fields) -> <<0:(lists:sum([S || {_, S} <- Fields]))>>.
c2(X) -> <<0:(begin N = X, N end)>>.
c3(X) -> <<0:(case X of 1 -> 8; N -> N end)>>.
c4(F, X) -> <<0:(F(fun(A) -> A + X end))>>.
p22(B) -> <<A:8, X:(byte_size(<<0:A>>))>> = B, X.
