-module(functionToReservedWord).
-export([foo/0]).

fo<caret>o() ->
  foo(),
  functionToReservedWord:foo().
