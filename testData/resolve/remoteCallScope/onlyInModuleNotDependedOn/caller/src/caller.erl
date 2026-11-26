-module(caller).
-export([g/0]).

g() -> dup:f().
