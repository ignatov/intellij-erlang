-module(exportLastComment).
-export([foo/0 % only for tests
        ]).

foo() -> bar(1).
bar<caret>(X) -> X.
