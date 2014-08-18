-export([f/0]).

f() ->
  <warning>Var</warning> = 1,
  fun (Var) -> Var  end.