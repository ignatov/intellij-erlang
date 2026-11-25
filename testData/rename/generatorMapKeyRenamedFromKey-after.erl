-module(generatorMapKeyRenamedFromKey).
-export([f/2]).

f(Ks, M) ->
  [K || Zz <- Ks, #{Zz<caret> := K} <- M].
