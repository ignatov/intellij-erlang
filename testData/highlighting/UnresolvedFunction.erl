-export([bar/0, fun_with_arity/0, op/0]).

-callback(foo(atom()) -> ok).

bar() ->
  <warning>foo</warning>().

%% no unresolved function warning should be issued
%% for references with unknown arity.

-export[fun_with_arity/<error>]</error>.

fun_with_arity() ->
  fun fun_with_arity/<error>.</error>

-spec spec<error>.</error>

%% operators
op() -> erlang:'<warning>=</warning>'(3, 3),
  hello:'<warning>></warning>'(1, 2),
  (fun erlang:'>'/2)(1, 2),
  erlang:'!'(1, 2),
  erlang:'<'/2,
  erlang:'<warning>orelse</warning>'(1, 2),
  'erlang':'<warning>-</warning>'(1, 1, 1),
  fun erlang:'-'/1,
  fun erlang:'bnot'/2,
  fun erlang:'xor'/2,
  fun erlang:'++'/2,
  fun 'erlang':'div'/2.