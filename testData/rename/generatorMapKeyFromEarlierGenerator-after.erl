-module(generatorMapKeyFromEarlierGenerator).
-export([f/2]).

f(Ks, M) ->
  [K || Zz<caret> <- Ks, #{Zz := K} <- M].
