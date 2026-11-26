f() ->
  <caret>Ok = ok,
  g(Ok),
  F = fun(ok) -> 1; (_) -> 2 end,
  F(Ok).
