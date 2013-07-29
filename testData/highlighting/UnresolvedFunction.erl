-export([bar/0]).

-callback(foo(atom()) -> ok).

bar() ->
  <warning>foo</warning>().