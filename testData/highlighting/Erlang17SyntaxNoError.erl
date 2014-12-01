-export([foo/0]).
foo() ->
  fun
    Foo() -> Foo()
  end,
  #{key => value}.