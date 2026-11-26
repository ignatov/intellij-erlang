-module(generatorExpressionVariable).
-export([g/1]).

g(<caret>L) ->
  [L || L <- L].
