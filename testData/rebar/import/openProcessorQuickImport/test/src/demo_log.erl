-module(demo_log).
-export([hello/0]).

hello() -> io:format("hello~n").
