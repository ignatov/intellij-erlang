-module(multiClauseFunction).
-export([foo/1]).

-spec foo(integer()) -> ok.
fo<caret>o(0) -> ok;
'foo'(N) when N > 0 ->
  foo(N - 1);
foo(N) ->
  multiClauseFunction:'foo'(-N).

foo() -> foo(1).
