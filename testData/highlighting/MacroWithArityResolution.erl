-export([foo/0, bar/0]).

-define(NULLARY_MACRO(), ok).
-define(UNARY_MACRO(X), ok).

foo() ->
  ?NULLARY_MACRO(),
  ?UNARY_MACRO(1).

bar() ->
  <error>?NULLARY_MACRO</error>,
  <error>?NULLARY_MACRO</error>(1),
  <error>?UNARY_MACRO</error>,
  <error>?UNARY_MACRO</error>(1, 2).
