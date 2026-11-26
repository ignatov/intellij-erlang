-module(eunit_not_first_include_lib_tests).

-include_lib("<error>kernel/include/file.hrl</error>").
-include_lib("eunit/include/eunit.hrl").

foo_test() ->
  ?assert(true).

<warning>bar</warning>() -> ok.
