%% Copyright
-module(<error>e_tests</error>).
-author("ignatov").

-include_lib("eunit/include/eunit.hrl").

get_num(A) -> A.

simple_test() ->
  ?assert(get_num(1) =:= 1).

basic_test_() ->
  fun () -> ?assert(1 + 1 =:= 2) end.

simple_2_test() ->
  ?assert(1 + 1 =:= 3).

<warning>foo</warning>() -> eunit:<warning>test</warning>(1).

zzz_test() -> ?assert(get_num(100) == 100).
