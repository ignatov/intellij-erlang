-module(underscoreMacro).

-define(Foo<caret>, 10).

foo() ->
  ?Foo.