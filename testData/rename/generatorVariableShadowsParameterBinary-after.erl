-module(generatorVariableShadowsParameterBinary).
-export([f/1]).
f(<caret>New) ->
    [X || <<X>> <= <<1, 2>>].
