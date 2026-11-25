f() ->
  g(<caret>ok),
  F = fun(ok) -> 1; (_) -> 2 end,
  F(ok).
