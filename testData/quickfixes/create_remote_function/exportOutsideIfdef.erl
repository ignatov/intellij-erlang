-module(exportOutsideIfdef).

foo() ->
  exportOutsideIfdef_target:f<caret>(1).
