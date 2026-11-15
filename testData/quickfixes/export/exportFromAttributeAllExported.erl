-module(exportFromAttributeAllExported).
-export([foo/0<caret>, bar/1]).

foo() -> bar(1).
bar(X) -> X.
