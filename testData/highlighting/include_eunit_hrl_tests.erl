-module(include_eunit_hrl_tests).

-include("eunit.hrl").

foo_test() ->
  ?assert(true).

<warning>bar</warning>() -> ok.
