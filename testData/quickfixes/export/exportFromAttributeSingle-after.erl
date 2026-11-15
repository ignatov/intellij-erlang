-module(exportFromAttributeSingle).
-export([foo/0, bar/1]).

foo() -> bar(1).
bar(X) -> X.
