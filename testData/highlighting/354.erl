-export([foo/1, foo/0]).
-record(baz, {}).
-spec(foo(#<error>bar</error>{}) ->  #baz{<error>a</error>:: 1, <error>b</error> :: 1}).
foo(#<error>bar</error>{}) -> #baz{}.

foo() ->
    baz,
    is_record(baz, baz),
    is_record(baz, <error>baz1</error>) .