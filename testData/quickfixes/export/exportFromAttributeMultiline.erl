-module(exportFromAttributeMultiline).
-export([
  foo/0, % api [v1]
  baz/0<caret>
]).

foo() -> bar(1).
bar(X) -> X.
baz() -> ok.
