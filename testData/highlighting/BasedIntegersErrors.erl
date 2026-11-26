-module('BasedIntegersErrors').
-export([b1/0, b2/0, b3/0, b4/0, b5/0, b6/0, b7/0, d1/0, d2/0, d3/0, d4/0, d5/0, d6/0, d7/0, d8/0, d9/0]).

b1() -> <error descr="illegal base '37'">37</error>#1.
b2() -> <error descr="illegal base '1'">1</error>#1.
b3() -> <error descr="illegal base '0'">0</error>#1.
b4() -> <error descr="illegal base '100'">100</error>#1.
b5() -> <error descr="illegal base '37'">3_7</error>#1.
b6() -> <error descr="illegal base '37'">0037</error>#1.
b7() -> <error descr="illegal base '100'">10_0</error>#1.
d1() -> 2#1<error descr="syntax error before: 2">2</error>.
d2() -> 2#1<error descr="syntax error before: _2">_2</error>.
d3() -> 16#F<error descr="syntax error before: _g">_g</error>.
d4() -> 16#FF<error descr="syntax error before: _Var">_Var</error>.
d5() -> 2#1_00<error descr="syntax error before: 2">2</error>.
d6() -> <error descr="illegal integer">2#2</error>.
d7() -> <error descr="illegal integer">8#8</error>.
d8() -> <error descr="illegal integer">10#A</error>.
d9() -> <error descr="illegal integer">2#3_1</error>.
