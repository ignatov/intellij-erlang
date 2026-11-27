-export([rematch/1, nested/0, named/0, head/1, guard/1, iff/1, rec/0, funguard/0]).

rematch(X) ->
  X = X,
  Y = 1,
  Y = Y,
  {A, B} = {Y, X},
  {P = [P]} = {X},
  {A, B, Y, P}.

nested() ->
  X = (Y = <error descr="Variable 'X' is unbound">X</error>),
  {X, Y}.

named() ->
  F = fun G(0) -> ok; G(N) -> G(N - 1) end,
  F(3).

head(V) ->
  case W = V of
    _ when W == V -> W
  end.

guard(X) when <error descr="Variable 'S' is unbound">S</error> == X ->
  S = X,
  S.

iff(X) ->
  if <error descr="Variable 'T' is unbound">T</error> == X -> T = X, T; true -> ok end.

rec() ->
  receive _ -> ok after <error descr="Variable 'U' is unbound">U</error> -> U = 1, U end.

funguard() ->
  F = fun(X) when <error descr="Variable 'Z' is unbound">Z</error> == X -> Z = X, Z end,
  F(1).
