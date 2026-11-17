-module(fixAll).

foo() ->
  fixAll_target:f<caret>(1),
  fixAll_target:g(2),
  fixAll_target:f(3).
