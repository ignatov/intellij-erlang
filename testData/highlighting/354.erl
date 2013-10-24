-export([foo/1]).
-record(baz, {}).
-spec(foo(#<error>bar</error>{}) ->  #baz{a:: 1, b :: 1}).
foo(#<error>bar</error>{}) -> #baz{}.