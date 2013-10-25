-export([get_profile/2, name_to_pid/1, do_receive/6, error/0, foo/0, foo2/0]).

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
          gen_server:<warning>call</warning>(Pid, {get_profile, DeviceID}); % mock it
        false ->
          lager:error("Cache worker is no longer alive! APIKey=~p", [APIKey]),
          invalid_pid
      end
  end.

-record(device_token, {bucket_id, apikey}).
-define(SCREEN, 1).
-define(TYPE_MULTIPART, 1).
-define(TYPE_JSON, 1).
-record(part, {value, content_type, value, content_type, value, content_type}).

do_receive(DeviceToken, ChannelPID, DeviceID, ChangeID, ChannelRef, WithAttachments) ->
  receive
    {ChannelPID, {ChannelRef, Members, Messages, Dest, CurrentCid, CreatedBy, CreateTime}} ->
      APIKey = DeviceToken#device_token.apikey,
      Output = [APIKey, {poll, ok}, ChannelPID, ChannelRef,
        DeviceID, Members, Messages, Dest, CurrentCid,
        CreatedBy, CreateTime, {}],
      io:format("receive : APIKey=~p, DeviceID=~p / (ChannelRef=~p : ChannelPID=~p) - Output=~p",
        [APIKey, DeviceID, ChannelRef, ChannelPID, Output]),

      JsonPart = Output,
      case WithAttachments of
        true ->
          BucketID = DeviceToken#device_token.bucket_id,
          ImageParts = [BucketID, ?SCREEN, Messages, []],
          {200, ?TYPE_MULTIPART, [#part{content_type=?TYPE_JSON, value=JsonPart} | ImageParts]};
        _ ->
          {200, ?TYPE_MULTIPART, [#part{content_type=?TYPE_JSON, value=JsonPart}]}
      end;
    {ChannelPID, timeout} ->
      JsonPart = [[{poll, timeout}]],
      {200, ?TYPE_MULTIPART, [#part{content_type=?TYPE_JSON, value=JsonPart}]};
    X ->
      lager:error("receive : DeviceID=~p, ChannelPID=~p/ChannelRef=~p, Failed to match pattern: ~p",
        [DeviceID, ChannelPID, ChannelRef, X]),
      erlang:error(unexpected_message, X)
  after 70000 ->
    lager:error("TIMEOUT! DeviceID=~p, ChannelRef=~p, ChangeID=~p, ChannelPID=~p",
      [DeviceID, ChannelRef, ChangeID, ChannelPID]),
    erlang:error(receive_timeout, 70000)
  end.

-export([find_in_binary/0, unbound_test/0]).

find_in_binary() ->
  case 1 of
    Last when Last < 0 -> Last;
    Last ->
      case 2 of
        _ -> Last
      end
  end.

unbound_test() ->
  <warning>Var1</warning> = <error>Var2</error>.

error()->
    {_FWver,_FWcrc,[_P0 | _Parts] = AllParts} = {1,1,[1,2,3]},
    AllParts.

foo() -> [{1,2} = Rec, {1,2}=Rec ] = [{1,2}, {1,2}].

foo2() -> [{1,2} = <warning>Rec</warning>] = [{1,2}].