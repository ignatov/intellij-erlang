-module(m).
-export([ foo/0 % c
        , bar/0
        , foo<caret>/0
        ]).

foo() -> ok.
bar() -> ok.
