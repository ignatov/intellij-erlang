-module(unquotedFunction).

bar() ->
  bar(),
  bar(),
  'unquotedFunction':bar(),
  unquotedFunction:bar().