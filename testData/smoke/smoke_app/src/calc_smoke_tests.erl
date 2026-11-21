-module(calc_smoke_tests).
-include_lib("eunit/include/eunit.hrl").

add_test() -> ?assertEqual(3, calc_smoke:add(1, 2)).
fail_test() -> ?assertEqual(5, calc_smoke:add(2, 2)).
env_test() -> ?assertEqual("hello world", os:getenv("SMOKE_ENV")).
