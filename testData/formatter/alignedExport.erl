%%  Copyright (c) 2012. Sergey Ignatov.
-module(aaa).
-author("ignatov").

%% API
-export([
foo/0,
                    bar/0
]).

foo() ->
  bar.

bar() ->
  foo.