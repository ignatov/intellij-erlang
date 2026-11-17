-module(exportFromAttributeSeveral).
-export([foo/0]).
-export([<caret>baz/0]).
-define(NAME, mac).

foo() -> bar(1) + 'my-fun'(2) + ?NAME().
bar(X) -> X.
baz() -> ok.
'my-fun'(X) -> X.
bar(X, Y) -> X + Y.
?NAME() -> 1.
