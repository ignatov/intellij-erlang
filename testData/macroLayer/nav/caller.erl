-module(caller).
-export([go/0]).

go() -> greet:hello().
