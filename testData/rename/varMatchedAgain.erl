-module(varMatchedAgain).
-export([f/0]).

f() ->
  A<caret> = 1,
  A = 1,
  A.
