-export([f/1, g/1]).
-define(PREFIX, "abc").
-define(ONE, 1).

f(?PREFIX ++ <error>"x" + 1</error>) -> ok;
f("abc" ++ <error>"x" + 1</error>) -> ok;
f("abc" ++ <error>[1] + 1</error>) -> ok;
f(<error>"x" + 1</error>) -> ok;
f(<error>?ONE + "x"</error>) -> ok;
f(<error>{1} + 1</error>) -> ok;
f(<error><<1>> + 1</error>) -> ok;
f(?ONE + 1) -> ok;
f(?ONE + ?ONE) -> ok;
f(1 + $a) -> ok;
f("abc" ++ [X]) -> X.

g(A) -> <error>"abc" ++ "x" + 1</error> = A.
