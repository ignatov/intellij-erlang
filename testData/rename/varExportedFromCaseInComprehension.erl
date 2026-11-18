-module(varExportedFromCaseInComprehension).
-export([f/1]).

f(L) ->
  [E || E <- L, case E of {X} -> true end, X<caret> > 1].
