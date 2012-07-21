-module(<error>test</error>).
-export([hello/1, <error>hello_world/1</error>, naive_reverse/1]).

naive_reverse([H|T]) ->
    naive_reverse(T)++[H];
naive_reverse([]) ->
    [].

hello_world() -> io:fwrite("hello, world").
hello_world2() -> io:fwrite("hello, world").

hello(X) -> X + 2.

