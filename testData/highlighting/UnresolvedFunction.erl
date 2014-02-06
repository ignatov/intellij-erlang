-export([bar/0, call_bar/0, call_unresolved/0, call_qualified_bar/0, call_qualified_unresolved/0, fun_with_arity/0]).

-callback(foo(atom()) -> ok).

bar() ->
  <warning>foo</warning>().

-define(CALL0(F), F()).

call_bar() ->
  ?CALL0(bar).

call_unresolved() ->
  <warning>?CALL0(unresolved)</warning>.

call_qualified_bar() ->
  ?CALL0('UnresolvedFunction':bar).

call_qualified_unresolved() ->
  <warning>?CALL0('UnresolvedFunction':unresolved)</warning>.

%% no unresolved function warning should be issued
%% for references with unknown arity.

-export[fun_with_arity/<error>]</error>.

fun_with_arity() ->
  fun fun_with_arity/<error>.</error>

-spec spec<error>.</error>