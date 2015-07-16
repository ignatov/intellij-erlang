-export([bar/0, fun_with_arity/0, foo/0]).

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
foo() -> erlang:'='(3, 3);
foo() -> (fun hello:<error>'>'</error>/2)(1, 2);
foo() -> (fun erlang:'>'/2)(1, 2);
foo() -> (fun erlang:'!'/2)(1, 2);
foo() -> fun erlang:'<'/2;
foo() -> (fun erlang:'orelse'/2)(1, 2);
foo() -> fun erlang:<error>'-'</error>/3;
foo() -> fun erlang:'-'/1;
foo() -> fun erlang:'bnot'/2;
foo() -> fun erlang:'xor'/2;
foo() -> fun erlang:'++'/2;
foo() -> fun erlang:'div'/2.