-module(unresolvedMacroInsideMacroBody).

-define(MACRO, ?UNRESOLVED, ok).

f() ->
  ?MACRO, ok.