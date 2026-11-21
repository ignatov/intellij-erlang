-export([f/1, g/1, h/1, i/1, j/1, k/1, ok/1]).
-record(r, {a}).

f(#{#{a => <error>"x" ++ "y"</error>} := V}) -> V.
g(#{[#{a => <error>"x" ++ "y"</error>}] := V}) -> V.
h(#{#r{a = <error>"a" ++ "b"</error>} := V}) -> V.
i(#{#{<error>"x" ++ "y"</error> => a} := V}) -> V.
j(#{{#r{a = <error>"a" ++ "b"</error>}} := V}) -> V.
k(#{#{a => #r{a = <error>"a" ++ "b"</error>}} := V}) -> V.

ok(#{a := #r{a = "a" ++ R}}) -> R;
ok(#r{a = "a" ++ R}) -> R;
ok(#{#r{a = 1} := V}) -> V;
ok(#{#{a => 1} := V}) -> V;
ok(#{a := #{b := "x" ++ R}}) -> R.
