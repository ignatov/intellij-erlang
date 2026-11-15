-module(varExportedFromCase).
-export([f/1]).

f(Y) ->
  case Y of {a, X} -> ok; {b, X} -> ok end,
  case Y of {c, X} -> X<caret> end.
