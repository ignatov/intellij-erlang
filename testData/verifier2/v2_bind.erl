-module(v2_bind).
-export([a/1, a2/1, b/1, c/2]).
-define(BIND(V, E), V = E).
-define(LET(V, E), V = E,).
-define(USE_Q, Q + 1).
-define(QC(P), P -> Q;).

a(X) ->
    ?BIND(Yy, X * 2),
    Yy + 1.

a2(X) ->
    ?LET(Zz, X * 3)
    Zz + 1.

b(Q) ->
    ?USE_Q.

c(X, Q) ->
    case X of
        ?QC(1)
        _ -> 0
    end.
