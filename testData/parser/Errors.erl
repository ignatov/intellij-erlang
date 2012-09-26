-module(errors).

-export([]).

f() ->
  1 +
  A = {abc, 1},
  {asd},
  lists: 1 + 1,
  dict : if true -> false end,
  if
      guard -> body ;
      guard -> body
  end,
  da
.

