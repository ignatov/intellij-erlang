-module(varExportedFromReceive).
-export([f/1]).

f(Y) ->
  receive {a, Zz} -> ok; {b, Zz} -> ok end,
  case Y of {c, Zz} -> Zz<caret> end.
