-module(generatorVariableShadowsParameterMap).
-export([f/1]).
f(<caret>X) ->
    [X || X := _ <- #{1 => 2}].
