-module(generatorMapKeyFromEarlierGenerator).
-export([f/2]).

f(Ks, M) ->
  [K || K<caret> <- Ks, #{K := K} <- M].
