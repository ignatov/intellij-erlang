-export([f/1, g/1, ok/1]).

f(<<(<error>"abc" ++ "x"</error> = X)/binary>>) -> X.
g(#{(<error>"a" ++ "b"</error> = X) := V}) -> {X, V}.

ok(X) -> {#{("a" ++ _ = X) => 1}, <<("b" ++ _ = X)/binary>>}.
