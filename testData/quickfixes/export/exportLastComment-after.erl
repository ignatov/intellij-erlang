-module(exportLastComment).
-export([foo/0, % only for tests
         bar/1
        ]).

foo() -> bar(1).
bar<caret>(X) -> X.
