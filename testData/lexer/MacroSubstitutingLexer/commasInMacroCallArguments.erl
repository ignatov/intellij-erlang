-module(commasInMacroCallArguments).


-define(RETURN(X), ok).

f(_X, _Y) -> ok.

%%% TODO add tests for other types of braces from ErlangBraceMatcher class

-define(RETURN_TUPLE(X, Y), ?RETURN({X, Y})).
-define(RETURN_CALL(X, Y), ?RETURN(f(X, Y))).
-define(RETURN_BIT(X, Y), ?RETURN(<<X, Y>>)).
-define(RETURN_BIT_COMPREHENSION(X, Y), ?RETURN(<< <<A + B>> || <<A>> <= <<X>>, <<B>> <= <<Y>> >>)).
-define(RETURN_LIST(X, Y), ?RETURN([X, Y])).
-define(RETURN_LIST_COMPREHENSION(X, Y), ?RETURN([ok || A <- [X], B <- [Y]])).

%%% end of macro definitions

g_tuple() -> ?RETURN_TUPLE(10, 10).
g_call() -> ?RETURN_CALL(10, 10).
g_bit() -> ?RETURN_BIT(10, 10).
g_bit_comprehension() -> ?RETURN_BIT_COMPREHENSION(10, 10).
g_list() -> ?RETURN_LIST(10, 10).
g_list_comprehension() -> ?RETURN_LIST_COMPREHENSION(10, 10).