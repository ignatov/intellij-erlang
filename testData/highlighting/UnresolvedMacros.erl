-define(resolved, 1).

-include_lib("eunit/include/eunit.hrl").

-export([foo/0, macro_test/0, use_known_macros_from_substitution/0, unresolved_after_substitution/0]).

foo() -> ?resolved + 1 + <error>?unresolved</error> + ?LINE.

macro_test() ->
    ?assert(true).

-define(KNOWN_MACROS, ?MACHINE, ?FILE, ?MODULE, ?MODULE_STRING, ?LINE).
use_known_macros_from_substitution() ->
    ?KNOWN_MACROS.

-define(ID(X), X).
unresolved_after_substitution() ->
    % we leave description here as we check that reported macro names are correct
    <error descr="Problem after macro substitution. Unresolved macro 'unresolved_atom_named_macro'">?ID</error>(?unresolved_atom_named_macro),
    <error descr="Problem after macro substitution. Unresolved macro 'Unresolved_var_named_macro'">?ID</error>(?Unresolved_var_named_macro).