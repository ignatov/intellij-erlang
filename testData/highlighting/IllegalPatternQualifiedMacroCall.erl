-export([c/2, d/1, bar/1]).
-define(F, bar).
-define(SIZE(X), X).

bar(X) -> X.

c(Y, X) -> {<error>X:?F(1)</error>} = Y, X.

d(<<X:?SIZE(8), _/binary>>) -> X.
