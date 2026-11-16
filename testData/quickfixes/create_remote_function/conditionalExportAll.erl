-module(conditionalExportAll).

foo() ->
  conditionalExportAll_target:f<caret>(1).
