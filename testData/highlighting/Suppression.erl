%% noinspection ErlangUnusedFunction
buzz() -> false.

%% noinspection ErlangUnboundVariable,ErlangUnusedVariable,ErlangUnusedFunction
buzz2(A, B, C) -> D.

%
<warning>buzz3</warning>(<warning>A</warning>, <warning>B</warning>, <warning>C</warning>) -> <error>D</error>.

%% noinspection ErlangUnusedFunction
f() ->
    %% noinspection ErlangUnresolvedFunction
    foo(0),
    %% noinspection ErlangUnboundVariable
    1 +   A,
    %% noinspection ErlangUnboundVariable
    D.
    
%%noinspection ErlangUnresolvedFunction
-define(MACROS(Val), (persistent_term:get(Val)):myfun()).

-define(MACROS(Val), (persistent_term:<warning>get</warning>(Val)):<warning>myfun</warning>()).
