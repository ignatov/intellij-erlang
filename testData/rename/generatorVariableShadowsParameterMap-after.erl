-module(generatorVariableShadowsParameterMap).
-export([f/1]).
f(<caret>New) ->
    [X || X := _ <- #{1 => 2}].
