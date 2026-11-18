-module(varInGeneratorBinarySize).
-export([f/1]).

f(B) ->
  N<caret> = 8,
  [{N, X} || <<X:N>> <= B].
