-module(unresolvedModule).

foo() ->
  no_such_module:lll<caret>(1).
