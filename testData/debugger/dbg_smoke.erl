-module(dbg_smoke).
-export([main/0]).

main() -> outer(10).
outer(A) -> B = A * 2, inner(B) + 1.
inner(C) ->
  D = C + 5,
  D.
