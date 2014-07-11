-module(unquoted<caret>Module).

foo() ->
  unquotedModule:foo(),
  'unquotedModule':foo().