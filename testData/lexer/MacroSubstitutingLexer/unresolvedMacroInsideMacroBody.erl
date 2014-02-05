-module(unresolvedMacroInsideMacroBody).

-define(MACRO, ?LINE, ok).

f() ->
  ?MACRO, ok.