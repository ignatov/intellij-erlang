-module(shop_defects).

%% Every form below plants one defect that both erlc and the plugin report; see ElpDiagnosticCounterparts.

-include("shop.hrl").
-include("no_such_header.hrl").
-include_lib("no_such_app/include/no_such_header.hrl").

-import(lists, [reverse/1, map/2]).
-import(shop_backend, [map/2]).
-import(shop_memory, [length/1]).

-export([unbound/0, calls_missing/0, calls_missing_remote/0, undefined_export/0, unused_variable/1]).
-export([unknown_record/0, unknown_field/0, unknown_macro/0, unknown_macro_call/0, reverse/1]).
-export([ambiguous/1, illegal_pattern/1, illegal_guard/1, bad_format/0, duplicate/0, duplicate/0]).

-type unused_type() :: atom().

-spec no_such_function() -> ok.

unbound() -> Unbound.

calls_missing() -> missing_local(1).

calls_missing_remote() -> shop_memory:no_such_function(1).

unused_function() -> ok.

unused_variable(Unused) -> ok.

unknown_record() -> #no_such_record{}.

unknown_field() -> #item{no_such_field = 1}.

unknown_macro() -> ?NO_SUCH_MACRO.

unknown_macro_call() -> ?NO_SUCH_MACRO_CALL(1).

reverse(List) -> List.

size(Term) -> Term.

ambiguous(Term) -> size(Term).

illegal_pattern(X + 1) -> X.

illegal_guard(X) when lists:member(X, [1, 2]) -> X.

bad_format() -> io:format("~p ~p~n", [one]).

duplicate() -> 1.
duplicate() -> 2.

mismatch(1) -> one;
mismatched(2) -> two.
