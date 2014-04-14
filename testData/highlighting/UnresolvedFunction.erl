-export([bar/0, fun_with_arity/0]).

-callback(foo(atom()) -> ok).

bar() ->
  <warning>foo</warning>().

%% no unresolved function warning should be issued
%% for references with unknown arity.

-export[fun_with_arity/<error>]</error>.

fun_with_arity() ->
  fun fun_with_arity/<error>.</error>

-spec spec<error>.</error>