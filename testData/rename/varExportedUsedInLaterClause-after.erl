-module(varExportedUsedInLaterClause).
-export([f/0]).

f() ->
  receive Zz<caret> -> Zz end,
  receive Y -> Zz = Y end,
  Zz.
