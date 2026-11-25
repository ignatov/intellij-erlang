-export([one/0, two/0]).

one() ->
  fun <warning descr="Unused variable 'Loop'">Loop</warning>(0) -> ok end.

two() ->
  fun <warning descr="Unused variable 'Loop'">Loop</warning>(0) -> ok; Loop(_) -> ok end.
