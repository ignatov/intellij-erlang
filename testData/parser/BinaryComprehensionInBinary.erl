-module(bincomp).
-export([f/1, g/1]).
f(L) -> << << <<C/utf8>> || C <- L>>/binary, "x">>.
g(L) -> << << <<C>> || C <- L>> >>.
