-module(varInGeneratorMapKey).
-export([f/1]).

f(L) ->
  Zz<caret> = a,
  [{Zz, V} || #{Zz := V} <- L].
