-module(a).
-export([hello/0]).
-define(DEF(Name), Name() -> ok).
?DEF(hello).
