-module(generatorVariableShadowsParameterBinary).
-export([f/1]).
f(<caret>X) ->
    [X || <<X>> <= <<1, 2>>].
