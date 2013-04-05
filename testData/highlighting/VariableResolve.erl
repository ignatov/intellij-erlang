-export([get_profile/2, name_to_pid/1]).

name_to_pid(Name) ->
  case Name of
    undefined ->
      case Name of
        undefined ->
          exit(could_not_find_registered_name);
            <warning>Pid</warning> -> null;
        <warning>Pid</warning> -> 1
      end;
    _ -> 1
  end.

warning(_What, _List) -> ok.

get_profile(APIKey, DeviceID) when is_binary(APIKey),
  is_binary(DeviceID) ->
  case ets:lookup(cache_workers, APIKey) of
    [] ->
      warning("Failed to lookup cache worker pid. APIKey=~p", [APIKey]),
      notfound;
    [{_, _, _, Pid}] ->
      case is_process_alive(Pid) of
        true ->
          gen_server:<warning>call</warning>(Pid, {get_profile, DeviceID}); % todo: mock it
        false ->
          lager:error("Cache worker is no longer alive! APIKey=~p", [APIKey]),
          invalid_pid
      end
  end.