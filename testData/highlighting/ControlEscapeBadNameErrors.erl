-module('ControlEscapeBadNameErrors').
-export([s1/0, s2/0, s3/0, g1/0, g2/1, g3/0, g4/0, g5/0, ok1/0, ok2/0]).

s1() -> "<error descr="illegal character">\^</error>"".
s2() -> "a<error descr="illegal character">\^</error>"b".
s3() -> "<error descr="illegal character">\^</error>"" ++ "b".
g1() -> ~"<error descr="illegal character">\^</error>"".
g2(F) -> F(~s(<error descr="illegal character">\^</error>).
g3() -> <<~b<<error descr="illegal character">\^</error>>>.
g4() -> {~s{<error descr="illegal character">\^</error>}.
g5() -> ~s/<error descr="illegal character">\^</error>/ 2.
ok1() -> ["\^\", "a\^\" ++ "b", ~"\^\", ~s(\^\), ~b<\^\>, ~s/\^\/, ~s'\^\', ~s{\^\}, ~s[\^]], "\^?\^a\^@"].
ok2() -> [~S"\^", ~B(\^), """
  \^"
  """].
