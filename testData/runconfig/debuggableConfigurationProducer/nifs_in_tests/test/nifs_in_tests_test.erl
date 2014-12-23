-module(nifs_in_tests_test).

-include_lib("eunit/include/eunit.hrl").

nifs_in_tests_test() -> erlang:load_nif(nifs_in_tests_test, 0).