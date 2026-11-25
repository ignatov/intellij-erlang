-module('SigilControlEscapeBadNameOtp25').
-export([g1/0, g2/1, g3/0, g4/0, g5/0, g6/1, ok1/0, ok2/0]).

g1() -> ~"<error descr="illegal character">\^</error>"".
g2(F) -> F(~s(<error descr="illegal character">\^</error>).
g3() -> <<~b<<error descr="illegal character">\^</error>>>.
g4() -> {~s{<error descr="illegal character">\^</error>}.
g5() -> ~s/<error descr="illegal character">\^</error>/ 2.
g6(F) -> F(~s(\x41<error descr="illegal character">\^</error>).
ok1() -> [~"\^\", ~s(\^\), ~b<\^\>, ~s/\^\/, ~s'\^\', ~s{\^\}, ~s[\^]], ~s"\^?\^a\^@"].
ok2() -> [~S"\^", ~B(\^), "\^1", "\^"", "\^\"].
