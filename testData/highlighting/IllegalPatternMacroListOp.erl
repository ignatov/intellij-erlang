-export([f/1, g/1, h/1, i/1, bar/0, bad/1]).
-define(PREFIX, "abc").
-define(PREFIX(X), X).

f(?PREFIX ++ Rest) -> Rest.

g(?PREFIX("abc") ++ Rest) -> Rest.

h(A) -> ?PREFIX ++ R = A, R.

i("x" ?PREFIX ++ Rest) -> Rest.

bar() -> "abc".

bad(<error>bar() ++ Rest</error>) -> Rest;
bad(<error>Prefix ++ Rest</error>) -> {Prefix, Rest}.
