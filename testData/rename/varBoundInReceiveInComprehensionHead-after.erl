-module(varBoundInReceiveInComprehensionHead).
-export([f/1]).

f(L) ->
  [receive X -> ok end || _ <- L],
  Zz = 1,
  Zz<caret>.
