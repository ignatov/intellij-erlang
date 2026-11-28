-module(eunit_noauto_tests).

-define(EUNIT_NOAUTO, true).
-include_lib("eunit/include/eunit.hrl").

<warning>foo_test</warning>() ->
  ?assert(true).
