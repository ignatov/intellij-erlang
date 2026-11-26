-export([f/0, g/0, h/0, i/0, j/0]).
-define(ARGS, X, Y).
f() ->
    fun <warning descr="Unused variable 'S'">S</warning>() -> 1; <error descr="Head mismatch: should have arity 0">S(X)</error> -> X end.
g() ->
    fun() -> 1; <error descr="Head mismatch: should have arity 0">(X)</error> -> X end.
h() ->
    fun(X) -> X; (Y) -> Y end.
i() ->
    fun(A, B) -> {A, B}; (?ARGS) -> ok end.
j() ->
    fun S() -> S; <error descr="Head mismatch: should be 'S'">T</error>(X) -> X end.
