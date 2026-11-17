-module(exportFromAttributeExportAll).
-compile(export_all).
-export([foo/0<caret>]).

foo() -> bar(1).
bar(X) -> X.
