-module(test).
-export([hello/1, hello_world/1, naive_reverse/1]).


naive_reverse([H|T]) ->
    naive_reverse(T)++[H];
naive_reverse([]) ->
    [].

hello_world() -> io:fwrite("hello, world").
hello_world2() -> io:fwrite("hello, world").

%-type eval() :: 'pos' | 'neg' | 'dont_know'.

hello(X) -> X + 2.
