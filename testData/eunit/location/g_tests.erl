-module(g_tests).
-include_lib("eunit/include/eunit.hrl").
plain_test() -> ok.
gen_test_() -> [?_assert(true),
  ?_assertEqual(1, 2)].
