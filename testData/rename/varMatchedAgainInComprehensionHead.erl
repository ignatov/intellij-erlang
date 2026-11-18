-module(varMatchedAgainInComprehensionHead).
-export([f/1]).

f(L) ->
  [fun() -> P<caret>id = self(), receive {Pid, R} -> Pid ! R end end || _ <- L].
