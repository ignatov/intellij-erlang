-module(m).
-export([ % c
        bar/0
        , foo<caret>/0
        ]).

foo() -> ok.
bar() -> ok.
