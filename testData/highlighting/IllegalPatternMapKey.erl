-export([f/1, g/1, h/1, i/1, j/1, k/1, l/1, m/1, n/1, ok/1]).
-define(PREFIX, "abc").

f(#{<error>[$a] ++ "b"</error> := V}) -> V.
g(#{<error>[1] ++ [2]</error> := V}) -> V.
h(#{<error>"a" ++ "b"</error> := V}) -> V.
i(#{<error>?PREFIX ++ "b"</error> := V}) -> V.
j(#{{<error>"a" ++ "b"</error>} := V}) -> V.
k(#{a := #{<error>"x" ++ "y"</error> := V}}) -> V.
l(X) -> #{<error>"a" ++ "b"</error> := V} = X, V.
m(X) -> case X of #{<error>"a" ++ "b"</error> := V} -> V end.
n(#{<error>"a" -- "b"</error> := V}) -> V.

ok(#{a := "x" ++ R}) -> R;
ok(#{1 + 1 := V}) -> V;
ok(#{"ab" := V}) -> V;
ok(#{[$a | "b"] := V}) -> V;
ok(X) -> #{"a" ++ "b" => X}.
