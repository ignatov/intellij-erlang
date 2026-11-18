-module(varInGeneratorMapKey).
-export([f/1]).

f(L) ->
  K<caret> = a,
  [{K, V} || #{K := V} <- L].
