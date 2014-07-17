-export([construct/1]).

set(_, _) -> ok.

construct(Values) ->
  S = ?MODULE_STRING,
  set(#?MODULE{}, Values ++ S).