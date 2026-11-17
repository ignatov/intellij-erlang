-module(exportFromAttributeMacro).
-define(F, foo).
-export([?F/0, bar/1]).

foo() -> bar(1).
bar(X) -> X.
