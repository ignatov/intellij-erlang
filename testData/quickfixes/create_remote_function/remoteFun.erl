-module(remoteFun).

foo() ->
  fun remoteFun_target:lll<caret>/2.
