-module(p_tests).
-include_lib("eunit/include/eunit.hrl").
v_test() -> ?assertEqual(script, p:v()).
