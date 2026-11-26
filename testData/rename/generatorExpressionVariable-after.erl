-module(generatorExpressionVariable).
-export([g/1]).

g(<caret>Zz) ->
  [L || L <- Zz].
