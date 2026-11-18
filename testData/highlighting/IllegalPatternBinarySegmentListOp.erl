-export([f/1, g/1, h/1, i/1, j/1, k/1, l/1, m/1, n/1, o/1, p/1, q/1, r/1, ok/1]).
-define(PREFIX, "abc").
-define(CH, $a).

f(<<(<error>[$a] ++ "x"</error>)/binary>>) -> ok.
g(<<(<error>?PREFIX ++ "x"</error>)/binary>>) -> ok.
h(<<(<error>?PREFIX ++ R</error>)/binary>>) -> R.
i(<<(<error>?MODULE_STRING ++ "x"</error>)/binary>>) -> ok.
j(<<(<error>[?CH] ++ "x"</error>)/binary>>) -> ok.
k(<<(<error>"abc" ++ "x"</error>)/binary>>) -> ok.
l(<<X:(<error>?PREFIX ++ "x"</error>)>>) -> X.
m(<<X:(<error>[$a] ++ "x"</error>)>>) -> X.
n(Y) -> <<(<error>"abc" ++ R</error>)/binary>> = Y, R.
o(Y) -> case Y of {<<(<error>?PREFIX ++ "x"</error>)/binary>>} -> ok end.
p("a" ++ <<(<error>"abc" ++ "x"</error>)/binary>>) -> ok.
q(Y) -> [ok || <<(<error>"abc" ++ "x"</error>):8>> <= Y].
r(<<(X = <error>"abc" ++ "x"</error>)/binary>>) -> X.

ok(<<X:8>>) -> X;
ok(X) -> <<("abc" ++ "x")/binary>> =:= X.
