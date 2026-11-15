-module(varMatchedAgainInComprehensionHead).
-export([f/1]).

f(L) ->
  [fun() -> Z<caret>z = self(), receive {Zz, R} -> Zz ! R end end || _ <- L].
