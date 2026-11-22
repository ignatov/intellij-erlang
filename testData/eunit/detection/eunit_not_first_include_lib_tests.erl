-module(eunit_not_first_include_lib_tests).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

foo_test() ->
  ?assert(true).
