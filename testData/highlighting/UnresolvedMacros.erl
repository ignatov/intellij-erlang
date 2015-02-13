-define(resolved, 1).

-include_lib("eunit/include/eunit.hrl").

-export([foo/0, macro_test/0, use_known_macros_from_substitution/0]).

foo() -> ?resolved + 1 + <error>?unresolved</error> + ?LINE.

macro_test() ->
    ?assert(true).

-define(KNOWN_MACROS, ?MACHINE, ?FILE, ?MODULE, ?MODULE_STRING, ?LINE).
use_known_macros_from_substitution() ->
    ?KNOWN_MACROS.