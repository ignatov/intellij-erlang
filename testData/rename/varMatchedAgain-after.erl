-module(varMatchedAgain).
-export([f/0]).

f() ->
  B<caret> = 1,
  B = 1,
  B.
