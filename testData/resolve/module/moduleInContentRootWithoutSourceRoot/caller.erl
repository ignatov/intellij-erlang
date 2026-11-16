-module(caller).

-export([foo/0]).

foo() ->
  cal<caret>lee:bar().
