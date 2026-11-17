-module(exportFromAttributeLastComment).
-export([
  foo/0, % starts it
  baz/0, % deprecated
  bar/1
]).

foo() -> bar(1).
bar(X) -> X.
baz() -> ok.
