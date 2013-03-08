-author("ignatov").

%% API
-export([name_to_pid/1]).

name_to_pid(Name) ->
  case Name of
    undefined ->
      case Name of
        undefined ->
          exit(could_not_find_registered_name);
        <warning>Pid</warning> -> null
      end;
    <warning>Pid</warning> -> 1
  end.