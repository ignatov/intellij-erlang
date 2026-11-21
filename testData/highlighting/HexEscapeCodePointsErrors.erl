-module('HexEscapeCodePointsErrors').
-export([c1/0, c2/0, c3/0, c4/0, c5/0, c6/0, a1/0, a2/0, s1/0, s2/0, s3/0, g1/0, g2/0, g3/0, g4/0, g5/0]).

c1() -> $<error descr="illegal character">\x{110000}</error>.
c2() -> $<error descr="illegal character">\x{D800}</error>.
c3() -> $<error descr="illegal character">\x{DFFF}</error>.
c4() -> $<error descr="illegal character">\x{FFFE}</error>.
c5() -> $<error descr="illegal character">\x{FFFF}</error>.
c6() -> $<error descr="illegal character">\x{FFFFFFFFFFFFFFFFFFFF}</error>.
a1() -> '<error descr="illegal character">\x{110000}</error>'.
a2() -> 'a<error descr="illegal character">\x{D800}</error>b'.
s1() -> "<error descr="illegal character">\x{110000}</error>".
s2() -> "a\\<error descr="illegal character">\x{D800}</error>b".
s3() -> <<"<error descr="illegal character">\x{FFFE}</error>">>.
g1() -> ~"<error descr="illegal character">\x{110000}</error>".
g2() -> ~s"<error descr="illegal character">\x{D800}</error>".
g3() -> ~b(<error descr="illegal character">\x{FFFF}</error>).
g4() -> ~b"""
  <error descr="illegal character">\x{110000}</error>
  """.
g5() -> ~s{<error descr="illegal character">\x{110000}</error>}.
