%%  Copyright (c) 2012. Sergey Ignatov.
-export([foo/0]).

-export([foo<caret>/0]).
-export([bar/0, tar/0]).

foo() -> ok.
bar() -> ok.
tar() -> ok.