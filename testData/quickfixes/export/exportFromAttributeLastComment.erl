-module(exportFromAttributeLastComment).
-export([
  foo/0, % starts it
  baz/0<caret> % deprecated
]).

foo() -> bar(1).
bar(X) -> X.
baz() -> ok.
