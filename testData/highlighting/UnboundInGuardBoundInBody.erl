-export([h/1]).

h(X) ->
  case X of
    _ when <error descr="Variable 'S' is unbound">S</error> == ok -> S = ok, S
  end.
