%% @spec read_chunk_length() -> integer()
%% @doc Read the length of the next HTTP chunk.
read_chunk_length() ->
  mochiweb_socket:setopts(Socket, [{packet, line}]),
  case mochiweb_socket:recv(Socket, 0, ?IDLE_TIMEOUT) of
    {ok, Header} ->
      mochiweb_socket:setopts(Socket, [{packet, raw}]),
      Splitter = fun (C) ->
        C =/= $\r andalso C =/= $\n andalso C =/= $
      end,
      {Hex, _Rest} = lists:splitwith(Splitter, binary_to_list(Header)),
      mochihex:to_int(Hex);
    _ ->
      exit(normal)
  end.

%% @spec parse_header(string()) -> {Type, [{K, V}]}
%% @doc  Parse a Content-Type like header, return the main Content-Type
%%       and a property list of options.
parse_header(String) ->
  %%       Should parse properly like mochiweb_cookies.
  [Type | Parts] = [string:strip(S) || S <- string:tokens(String, ";")],
  F = fun (S, Acc) ->
    case lists:splitwith(fun (C) -> C =/= $= end, S) of
      {"", _} ->
        %% Skip anything with no name
        Acc;
      {_, ""} ->
        %% Skip anything with no value
        Acc;
      {Name, [$\= | Value]} ->
        [{string:to_lower(string:strip(Name)),
          unquote_header(string:strip(Value))} | Acc]
    end
  end,
  {string:to_lower(Type),
    lists:foldr(F, [], Parts)}.

ctype($\%) -> percent;
ctype($\,) -> comma;
ctype($\.) -> dot;
ctype($\_) -> underscore;
ctype($s) -> string;
ctype($b) -> bin;
ctype($o) -> oct;
ctype($X) -> upper_hex;
ctype($x) -> hex;
ctype($c) -> char;
ctype($d) -> decimal;
ctype($g) -> general;
ctype($f) -> fixed;
ctype($e) -> exp.