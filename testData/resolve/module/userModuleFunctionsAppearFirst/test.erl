-module(test).

-export([foo/0]).

foo() ->
  eunit:te<caret>st(x).
