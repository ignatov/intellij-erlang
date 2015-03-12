-export([construct/1]).

set(_, _) -> ok.

construct(Values) ->
  S = ?MODULE_STRING,
  set(#?UNKNOWN_MACRO{}, Values ++ S).