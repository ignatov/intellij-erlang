-module(exportFromAttributeCommaFirst).
-export([ foo/0
        , baz/0<caret>
        ]).

foo() -> bar(1).
bar(X) -> X.
baz() -> ok.
