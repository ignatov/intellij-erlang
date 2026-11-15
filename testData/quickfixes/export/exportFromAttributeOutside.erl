-module(exportFromAttributeOutside).
-export([foo/0]).
-author(<caret>"me").

foo() -> bar(1).
bar(X) -> X.
