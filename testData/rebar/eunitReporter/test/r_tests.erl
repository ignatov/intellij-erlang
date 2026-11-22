-module(r_tests).
-include_lib("eunit/include/eunit.hrl").
passing_test() -> ?assertEqual(2, r:two()).
failing_test() -> ?assertEqual(3, r:two()).
