-module(m).
-export([% c
         bar/0]).
-export([foo<caret>/0]).

foo() -> ok.
bar() -> ok.
