-module(dz_smoke).
-export([f/0]).

f() -> g(1).

g(X) when is_atom(X) -> X.
