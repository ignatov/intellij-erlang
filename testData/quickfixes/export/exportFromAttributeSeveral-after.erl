-module(exportFromAttributeSeveral).
-export([foo/0]).
-export([baz/0, bar/1, 'my-fun'/1, bar/2]).
-define(NAME, mac).

foo() -> bar(1) + 'my-fun'(2) + ?NAME().
bar(X) -> X.
baz() -> ok.
'my-fun'(X) -> X.
bar(X, Y) -> X + Y.
?NAME() -> 1.
