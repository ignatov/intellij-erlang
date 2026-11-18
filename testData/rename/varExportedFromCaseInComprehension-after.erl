-module(varExportedFromCaseInComprehension).
-export([f/1]).

f(L) ->
  [E || E <- L, case E of {Zz} -> true end, Zz<caret> > 1].
