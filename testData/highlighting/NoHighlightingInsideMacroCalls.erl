-module('NoHighlightingInsideMacroCalls').

-export([test/0]).

-record(rec, {f1}).

-define(IGNORE(What), ok).

test() ->
  ?IGNORE(#{a => b}),
  ?IGNORE(fun Foo() -> Foo() end),
  ?IGNORE(fun Foo() -> Foo(); Bar() -> Foo() end),
  ?IGNORE(io:format("~n", [1, 2, 3])),
  ?IGNORE(io_lib:format("~n", [1, 2, 3])),
  ?IGNORE(UnboundVariable),
  ?IGNORE(fun () -> UnusedVariable = 10 end),
  ?IGNORE(unresolved_function()),
  ?IGNORE(fun unresolved_function/0),
  ?IGNORE(?UNRESOLVED_MACRO),
  ?IGNORE(#unresolved_record{}),
  ?IGNORE(#rec{unresolved_record_field = 0}).