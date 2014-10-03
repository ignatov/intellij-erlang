-module(stringifyMacroArgument).

-define(LOG_EXPR(EXPR),
  case EXPR of
    Result ->
      io:format("~s: ~p~n", [??EXPR, Result]),
      Result
  end).

log_2_plus_2() ->
  ?LOG_EXPR(2 + 2).