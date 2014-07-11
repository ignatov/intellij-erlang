-module('quoted<caret>Module').

foo() ->
  quotedModule:foo(),
  'quotedModule':foo().