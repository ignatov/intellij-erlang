-module('UnboundVariable').

-export([foo/0, bar/0]).

bar() ->
  <error>X</error>.

-define(VARX0, X = 0).

foo() ->
  ?VARX0,
  X.