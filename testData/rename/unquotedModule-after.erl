-module('unquotedModule-after').

foo() ->
  'unquotedModule-after':foo(),
  'unquotedModule-after':foo().