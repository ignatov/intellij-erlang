-export([f/1, g/1]).

f(M) ->
  G = fun g/1,
  F = g,
  A = 1,
  _ = lists:map(fun <error descr="A fun with a variable function name needs a module: fun M:F/Arity">G</error>/1, []),
  _ = fun <error descr="A fun with a variable function name needs a module: fun M:F/Arity">F</error>/0,
  _ = fun <error descr="A fun with a variable function name needs a module: fun M:F/Arity">F</error>/A,
  _ = fun M:F/A,
  _ = fun M:g/1,
  _ = fun erlang:F/1,
  ok.

g(_) -> ok.
