-module(exportFromAttributeMultiline).
-export([
  foo/0, % api [v1]
  baz/0,
  bar/1
]).

foo() -> bar(1).
bar(X) -> X.
baz() -> ok.
