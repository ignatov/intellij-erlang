-module(exportFromAttributeTrailingCommaComment).
-export([
  foo/0, % c ]
  <caret>
]).

foo() -> bar(1).
bar(X) -> X.
