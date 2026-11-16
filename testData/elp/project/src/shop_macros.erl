-module(shop_macros).

%% erlc compiles this module. ELP 1.1.0's own parser reports a syntax error (P1711) on a macro that expands to the head of
%% a catch clause, as epgsql and rebar3 define it, and on its use.

-export([safely/1]).

-define(WITH_STACKTRACE(Class, Reason, Stacktrace), Class:Reason:Stacktrace ->).

safely(Fun) ->
    try Fun()
    catch
        ?WITH_STACKTRACE(Class, Reason, Stacktrace)
            {error, {Class, Reason, Stacktrace}}
    end.
