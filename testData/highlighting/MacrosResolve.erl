-export([construct/1]).

set(_, _) -> ok.

construct(Values) ->
  S = ?MODULE_STRING,
  F = ?FUNCTION_NAME,
  A = ?FUNCTION_ARITY ,
  set(#?MODULE{}, Values ++ S ++ F ++ A).