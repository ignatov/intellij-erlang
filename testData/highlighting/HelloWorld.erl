-module(<error>test</error>).
-export([hello/1, <warning>hello_world/1</warning>, naive_reverse/1]).

naive_reverse([H|T]) ->
    naive_reverse(T)++[H];
naive_reverse([]) ->
    [].

<warning>hello_world</warning>() -> io:fwrite("hello, world").
<warning>hello_world2</warning>() -> io:fwrite("hello, world").

hello(X) -> X + 2.

