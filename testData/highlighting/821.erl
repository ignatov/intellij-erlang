-export([f/1, g/1, h/0, k/0]).
-define(FUN(F), fun F/1).
-define(FA(F, A), fun F/A).

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

h() -> ?FUN(g).
k() -> ?FA(g, 1).

g(_) -> ok.
