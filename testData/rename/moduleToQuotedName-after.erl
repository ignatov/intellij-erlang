-module('moduleToQuotedName-after').

foo() ->
  'moduleToQuotedName-after':foo(),
  'moduleToQuotedName-after':foo().