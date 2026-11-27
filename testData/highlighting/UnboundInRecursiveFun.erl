-export([f/0]).

f() ->
  F = fun(0) -> ok; (N) -> <error descr="Variable 'F' is unbound">F</error>(N - 1) end,
  F(3).
