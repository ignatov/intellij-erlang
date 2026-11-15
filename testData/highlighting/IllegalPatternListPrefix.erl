-export([f/1, g/1, h/1]).
-define(PREFIX, "abc").
-define(CH, $a).
-define(F, bar).

f([$a] ++ Rest) -> Rest;
f([?CH] ++ Rest) -> Rest;
f(("abc") ++ Rest) -> Rest;
f((?PREFIX) ++ Rest) -> Rest;
f([] ++ Rest) -> Rest;
f([1, 2] ++ Rest) -> Rest;
f([16#41, ($b)] ++ Rest) -> Rest;
f([$a | "bc"] ++ Rest) -> Rest;
f([$a | ?PREFIX] ++ Rest) -> Rest;
f([$a | [$b]] ++ Rest) -> Rest;
f(([$a]) ++ Rest) -> Rest;
f([$a] ++ [$b] ++ Rest) -> Rest;
f(<error>[-1] ++ Rest</error>) -> Rest;
f(<error>[1 + 1] ++ Rest</error>) -> Rest;
f(<error>[a] ++ Rest</error>) -> Rest;
f(<error>[1.0] ++ Rest</error>) -> Rest;
f(<error>["a"] ++ Rest</error>) -> Rest;
f(<error>[X] ++ Rest</error>) -> {X, Rest};
f(<error>[$a | X] ++ Rest</error>) -> {X, Rest};
f(<error>[{}] ++ Rest</error>) -> Rest;
f(<error>16#41 ++ Rest</error>) -> Rest;
f(<error><<"a">> ++ Rest</error>) -> Rest.

g(A) -> [$a] ++ R = A, R.

h(<error>X:?F() ++ Rest</error>) -> {X, Rest}.
