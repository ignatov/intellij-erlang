-export([foo/0]).
foo() ->
  fun
    <error>Foo</error>() -> Foo()
  end,
  <error>#{key => value}</error>.