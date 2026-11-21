-module(macroMoreClauses).
-export([g/1]).

-define(MORE, ; b -> 2).

g(X) -> case X of a -> 1 ?MORE<error descr="'end' unexpected"> </error>end.
