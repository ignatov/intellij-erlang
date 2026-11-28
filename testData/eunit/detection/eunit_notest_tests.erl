-module(eunit_notest_tests).

-define(NOTEST, true).
-include_lib("eunit/include/eunit.hrl").

foo_test() ->
  ?assert(true).
