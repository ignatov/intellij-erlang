-module(quotedFunction).

'fo<caret>o'() ->
  foo(),
  'foo'(),
  'quotedFunction':foo(),
  quotedFunction:'foo'().