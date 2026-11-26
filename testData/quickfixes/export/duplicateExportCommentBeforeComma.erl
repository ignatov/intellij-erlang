-module(m).
-export([foo/0
         % c
         , bar/0]).
-export([foo<caret>/0]).

foo() -> ok.
bar() -> ok.
