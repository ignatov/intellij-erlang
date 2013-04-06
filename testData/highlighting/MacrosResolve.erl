-export([construct/1]).

set(_, _) -> ok.

construct(Values) ->
  set(#?MODULE{}, Values).