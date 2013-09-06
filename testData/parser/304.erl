%% Copyright
-module(test1).
-author("ignatov").

%% API

-ifdef(DM).
-define(D(S), io:format("dbg" ++ S)).
-else
-define(D(S), ok).
-endif.