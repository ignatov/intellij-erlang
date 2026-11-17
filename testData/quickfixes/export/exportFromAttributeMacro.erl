-module(exportFromAttributeMacro).
-define(F, foo).
-export([?F/0<caret>]).

foo() -> bar(1).
bar(X) -> X.
