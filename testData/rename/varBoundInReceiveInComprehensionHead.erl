-module(varBoundInReceiveInComprehensionHead).
-export([f/1]).

f(L) ->
  [receive X -> ok end || _ <- L],
  X = 1,
  X<caret>.
