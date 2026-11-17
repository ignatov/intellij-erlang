-module(anonymousFunLaterClauseVariable).
-export([a/0]).

a() ->
  fun (0) -> ok;
      (1) -> <caret>Zz = 1, Zz
  end.
