-module(a).
-export([f/1]).

f(L) when length(L) > 0 -> {<selection>length(L)</selection>, length(L)}.
