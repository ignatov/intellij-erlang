-module(generatorMapKeyRenamedFromKey).
-export([f/2]).

f(Ks, M) ->
  [K || K <- Ks, #{K<caret> := K} <- M].
