-export([one/0, two/0, three/0, four/0, five/0]).

one() ->
  fun <warning descr="Unused variable 'Loop'">Loop</warning>(0) -> ok end.

two() ->
  fun <warning descr="Unused variable 'Loop'">Loop</warning>(0) -> ok; Loop(_) -> ok end.

three() ->
  fun L(0) -> ok; L(N) -> L(N - 1) end.

four() ->
  fun <warning descr="Unused variable 'L'">L</warning>(0, _) -> ok; L(_, L) -> L end.

five() ->
  fun L(0) -> ok; L(_) -> ok; L(N) -> L(N - 1) end.
