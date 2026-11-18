-module(varInGeneratorBinarySize).
-export([f/1]).

f(B) ->
  Zz<caret> = 8,
  [{Zz, X} || <<X:Zz>> <= B].
