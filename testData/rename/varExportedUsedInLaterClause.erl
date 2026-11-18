-module(varExportedUsedInLaterClause).
-export([f/0]).

f() ->
  receive X<caret> -> X end,
  receive Y -> X = Y end,
  X.
