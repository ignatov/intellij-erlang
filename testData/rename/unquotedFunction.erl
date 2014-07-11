-module(unquotedFunction).

fo<caret>o() ->
  foo(),
  'foo'(),
  'unquotedFunction':foo(),
  unquotedFunction:'foo'().