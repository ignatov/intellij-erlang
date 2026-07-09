%%  Copyright (c) 2012. Sergey Ignatov.
-module(aaa).
-author("ignatov").

%% API
-import(lists, [
  nth/2,
  all/2
]).

test() ->
  nth(1, [1, 2, 3]).