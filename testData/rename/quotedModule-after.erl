-module('quotedModule-after').

foo() ->
  'quotedModule-after':foo(),
  'quotedModule-after':foo().