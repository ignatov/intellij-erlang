%% The erlc oracle of the property-based tests (tests/org/intellij/erlang/pbt).
%% One long-lived process: it reads one file path per line on standard input and writes one JSON line per file on
%% standard output with every error and warning that compile:file/2 reports for it. The first line it writes is a
%% handshake with the OTP release. Needs the json module (OTP 27 or newer).
-module(pbt_oracle).
-export([main/0]).

main() ->
    ok = io:setopts(standard_io, [binary, {encoding, unicode}]),
    emit(#{<<"hello">> => true, <<"otp">> => list_to_binary(erlang:system_info(otp_release))}),
    loop().

loop() ->
    case io:get_line(standard_io, '') of
        eof -> halt(0);
        {error, _} -> halt(1);
        Line ->
            Path = string:trim(Line),
            case Path of
                <<>> -> loop();
                _ -> emit(check(unicode:characters_to_list(Path))), loop()
            end
    end.

emit(Term) ->
    io:put_chars(standard_io, [json:encode(Term), $\n]).

options() ->
    [return_errors, return_warnings, binary, strong_validation, {error_location, column}].

check(Path) ->
    {Status, Errors, Warnings} =
        try compile:file(Path, options()) of
            {ok, _Mod, Ws} -> {ok, [], Ws};
            {ok, _Mod, _Bin, Ws} -> {ok, [], Ws};
            {error, Es, Ws} -> {error, Es, Ws};
            error -> {error, [], []};
            Other -> {crash, [{Path, [{none, ?MODULE, {unexpected, Other}}]}], []}
        catch
            Class:Reason -> {crash, [{Path, [{none, ?MODULE, {Class, Reason}}]}], []}
        end,
    #{<<"file">> => unicode:characters_to_binary(Path),
      <<"status">> => atom_to_binary(Status),
      <<"errors">> => flatten(Errors),
      <<"warnings">> => flatten(Warnings)}.

flatten(PerFile) ->
    [item(Loc, Mod, Desc) || {_File, Items} <- PerFile, {Loc, Mod, Desc} <- Items].

item(Loc, Mod, Desc) ->
    {Line, Col} = case Loc of
                      {L, C} when is_integer(L), is_integer(C) -> {L, C};
                      L when is_integer(L) -> {L, 0};
                      _ -> {0, 0}
                  end,
    Base = #{<<"module">> => atom_to_binary(Mod),
             <<"line">> => Line,
             <<"column">> => Col,
             <<"tag">> => tag(Desc),
             <<"message">> => message(Mod, Desc)},
    case name(Desc) of
        none -> Base;
        Name -> Base#{<<"name">> => Name}
    end.

tag(Desc) when is_tuple(Desc), tuple_size(Desc) > 0, is_atom(element(1, Desc)) -> atom_to_binary(element(1, Desc));
tag(Desc) when is_atom(Desc) -> atom_to_binary(Desc);
tag(_) -> <<"message">>.

%% The name the descriptor is about: a variable, a record or {Name, Arity}.
name(Desc) when is_tuple(Desc), tuple_size(Desc) >= 2 -> name_of(element(2, Desc));
name(_) -> none.

name_of(A) when is_atom(A) -> atom_to_binary(A);
name_of({F, A}) when is_atom(F), is_integer(A) -> <<(atom_to_binary(F))/binary, $/, (integer_to_binary(A))/binary>>;
name_of({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) ->
    <<(atom_to_binary(M))/binary, $:, (atom_to_binary(F))/binary, $/, (integer_to_binary(A))/binary>>;
name_of(_) -> none.

message(Mod, Desc) ->
    try unicode:characters_to_binary(io_lib:format("~ts", [Mod:format_error(Desc)]))
    catch _:_ -> unicode:characters_to_binary(io_lib:format("~0tp", [Desc]))
    end.
