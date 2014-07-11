-module(quotedFunction).

bar() ->
  bar(),
  bar(),
  'quotedFunction':bar(),
  quotedFunction:bar().