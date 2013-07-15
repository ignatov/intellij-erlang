-define(resolved, 1).

-include_lib("eunit/include/eunit.hrl").

-export([foo/0, macro_test/0]).

foo() -> ?resolved + 1 + <error>?unresolved</error> + ?LINE.

macro_test() ->
    ?assert(true).