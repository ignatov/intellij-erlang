%%  Copyright (c) 2012. Sergey Ignatov.
-export([foo/0, bar/0, foo/0, foo/0, tar/0, bar/0]).
-export([bar/0, bar/0<caret>, foo/0]).

foo() -> ok.
bar() -> ok.
tar() -> ok.