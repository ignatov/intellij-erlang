-export([f/0, g/0, h/0]).
f() ->
    fun S() -> 1; <error>S(X)</error> -> X end.
g() ->
    fun() -> 1; <error>(X)</error> -> X end.
h() ->
    fun(X) -> X; (Y) -> Y end.
