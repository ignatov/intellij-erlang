-export([bar/0]).

updater() ->
    ok.

foo(Func) ->
    Func().

bar() ->
    foo(fun updater/0).