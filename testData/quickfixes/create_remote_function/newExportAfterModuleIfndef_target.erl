-ifndef(X).
-define(X, 1).
-endif.
-module(newExportAfterModuleIfndef_target).

bar() -> ok.
