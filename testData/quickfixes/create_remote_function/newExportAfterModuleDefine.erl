-module(newExportAfterModuleDefine).

foo() ->
  newExportAfterModuleDefine_target:f<caret>(1).
