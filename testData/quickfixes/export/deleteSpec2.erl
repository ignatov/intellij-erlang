%%  Copyright (c) 2012. Sergey Ignatov.
-module(empty).
-author("ignatov").

%% API
-export([bar/10, zoo/2]).

-spec(foo() ->  ok).

-spec bar() -> atom().
bar() -> ok.

foo<caret>() -> ok.
