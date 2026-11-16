-module(exportOutsideIf).

foo() ->
  exportOutsideIf_target:f<caret>(1).
