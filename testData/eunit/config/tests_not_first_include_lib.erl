-module(tests_not_first_include_lib).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

foo_test() ->
  ?assert(true).
