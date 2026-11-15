-module(varExportedFromCase).
-export([f/1]).

f(Y) ->
  case Y of {a, Zz} -> ok; {b, Zz} -> ok end,
  case Y of {c, Zz} -> Zz<caret> end.
