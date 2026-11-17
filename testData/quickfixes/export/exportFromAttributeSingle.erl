-module(exportFromAttributeSingle).
-export([foo/0<caret>]).

foo() -> bar(1).
bar(X) -> X.
