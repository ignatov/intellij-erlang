-module(newExportOutsideIfdef).

foo() ->
  newExportOutsideIfdef_target:f<caret>(1).
