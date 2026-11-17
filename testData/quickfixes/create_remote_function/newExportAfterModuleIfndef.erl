-module(newExportAfterModuleIfndef).

foo() ->
  newExportAfterModuleIfndef_target:f<caret>(1).
