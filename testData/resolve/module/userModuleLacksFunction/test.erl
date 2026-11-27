-module(test).

-export([foo/0]).

foo() ->
  eunit:st<caret>art().
