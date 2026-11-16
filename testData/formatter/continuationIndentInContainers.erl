-module(continuationIndentInContainers).
-export([foo/2]).

foo(A, B) ->
  L = [A,
  B],
  T = {A,
  B},
  lists:append(L,
  [T]).
