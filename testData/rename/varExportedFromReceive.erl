-module(varExportedFromReceive).
-export([f/1]).

f(Y) ->
  receive {a, X} -> ok; {b, X} -> ok end,
  case Y of {c, X} -> X<caret> end.
