-module(anonymousFunLaterClauseVariable).
-export([a/0]).

a() ->
  fun (0) -> ok;
      (1) -> <caret>X = 1, X
  end.
