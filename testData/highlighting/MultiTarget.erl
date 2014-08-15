-export([f/0]).

f() ->
  case dothing() of
    <warning>V1</warning> ->
      Bar = 1;
    _ ->
      Bar = 2
  end,
  Bar.

dothing() ->
  true.
