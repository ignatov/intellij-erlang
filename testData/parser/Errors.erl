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

not_add() -> ok|.

list1() -> [first,
.
list2() -> [first|
.
list3() -> [first||
.