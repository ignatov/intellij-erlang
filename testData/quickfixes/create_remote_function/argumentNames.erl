-module(argumentNames).

foo(X) ->
  argumentNames_target:f<caret>(X, #{}, $a, <<>>, 1, 2, X, 'hello world', "s").
