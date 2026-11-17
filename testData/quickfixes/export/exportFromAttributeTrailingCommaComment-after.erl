-module(exportFromAttributeTrailingCommaComment).
-export([
  foo/0, % c ]
  bar/1
]).

foo() -> bar(1).
bar(X) -> X.
