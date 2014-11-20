%%  Copyright (c) 2012. Sergey Ignatov.
-export([foo/0, bar/0, foo/0, foo/0<caret>, tar/0]).

foo() -> ok.
bar() -> ok.
tar() -> ok.