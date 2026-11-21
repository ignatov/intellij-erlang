#!/usr/bin/env escript
%% Compares the plugin's macro expansion with epp.
%% usage: epp_compare.escript <module.erl> <plugin forms file>
%% The plugin file has one line per expanded form: "<line>\t<base64 of the expanded form text>".
%% Prints "OK <line>" or "DIFF <line>\t<plugin tokens>\t<epp tokens>" per form, and "NOEPP <line>" when epp has no form
%% that starts on that line (a skipped branch, an error).
-mode(compile).

main([File, PluginFile]) ->
    {ok, EppForms, _} = epp:scan_file(File, []),
    Ordered = [{erl_anno:line(element(2, hd(T))), norm(T)} || T <- EppForms, is_list(T), T =/= [], is_tuple(hd(T))],
    ByLine = maps:from_list([{L, rest_from(L, Ordered)} || {L, _} <- Ordered]),
    {ok, Bin} = file:read_file(PluginFile),
    Lines = [L || L <- binary:split(Bin, <<"\n">>, [global]), L =/= <<>>],
    lists:foreach(fun(L) -> compare(L, ByLine) end, Lines);
main(_) ->
    io:format("usage: epp_compare.escript <module.erl> <plugin forms file>~n"),
    halt(2).

%% the epp forms from the one on line L on: a dot inside an expansion splits a form for epp
rest_from(L, [{L, _} | _] = Forms) -> [T || {_, T} <- Forms];
rest_from(L, [_ | Rest]) -> rest_from(L, Rest).

compare(L, ByLine) ->
    [LineB, B64] = binary:split(L, <<"\t">>),
    Line = binary_to_integer(LineB),
    Text = binary_to_list(base64:decode(B64)),
    case maps:find(Line, ByLine) of
        error -> io:format("NOEPP ~p~n", [Line]);
        {ok, [First | _] = EppForms} ->
            case erl_scan:string(Text, {Line, 1}) of
                {ok, Toks, _} ->
                    Plugin = norm(Toks),
                    case match_joined(Plugin, [], EppForms) of
                        true -> io:format("OK ~p~n", [Line]);
                        false ->
                            Joined = join_upto(length(Plugin), [], EppForms),
                            I = case mismatch(Plugin, Joined, 1) of none -> first_diff(Plugin, Joined, 1); X -> X end,
                            io:format("DIFF ~p\tat token ~p: plugin ~w\tepp ~w~n",
                                      [Line, I, window(Plugin, I), window(Joined, I)])
                    end;
                {error, E, _} -> io:format("DIFF ~p\tscan-error ~w\t~w~n", [Line, E, First])
            end
    end.

%% a '.' that ends a macro body is a dot once a blank follows it in the plugin's text: the same token here
norm(Toks) -> [case tok(T) of '.' -> dot; X -> X end || T <- Toks].

tok({Cat, _}) -> Cat;
tok({Cat, _, V}) -> {Cat, V}.

-define(PREDEFINED, ['MODULE', 'MODULE_STRING', 'FILE', 'LINE', 'MACHINE', 'FUNCTION_NAME', 'FUNCTION_ARITY', 'OTP_RELEASE']).

join_upto(N, Acc, [F | Rest]) when length(Acc) < N -> join_upto(N, Acc ++ F, Rest);
join_upto(_, Acc, _) -> Acc.

first_diff([T | P], [T | E], I) -> first_diff(P, E, I + 1);
first_diff(_, _, I) -> I.

window(L, I) -> lists:sublist(L, max(1, I - 4), 12).

%% the plugin's form against one epp form, or against several in a row
match_joined(Plugin, Acc, [F | Rest]) ->
    Joined = Acc ++ F,
    match(Plugin, Joined) orelse (length(Joined) < length(Plugin) andalso match_joined(Plugin, Joined, Rest));
match_joined(_, _, []) -> false.

%% the plugin may leave a predefined macro as it is: it matches any one token of epp, and
%% ?FEATURE_AVAILABLE(F) / ?FEATURE_ENABLED(F) match any run of epp tokens
match(P, E) -> mismatch(P, E, 1) =:= none.

%% none, or the index (in the plugin's tokens) where the match fails
mismatch([], [], _) -> none;
mismatch(['?', {var, N}, '(' | P], E, I) when N =:= 'FEATURE_AVAILABLE'; N =:= 'FEATURE_ENABLED' ->
    Rest = skip_args(P, 1),
    Skipped = length(P) - length(Rest) + 3,
    case lists:any(fun(K) -> mismatch(Rest, lists:nthtail(K, E), I + Skipped) =:= none end, lists:seq(1, length(E))) of
        true -> none;
        false -> I
    end;
mismatch(['?', {Cat, N} | P], [_ | E], I) when Cat =:= var; Cat =:= atom ->
    case lists:member(N, ?PREDEFINED) of
        true -> mismatch(P, E, I + 2);
        false -> I
    end;
mismatch([T | P], [T | E], I) -> mismatch(P, E, I + 1);
mismatch(_, _, I) -> I.

skip_args(P, 0) -> P;
skip_args(['(' | P], D) -> skip_args(P, D + 1);
skip_args([')' | P], D) -> skip_args(P, D - 1);
skip_args([_ | P], D) -> skip_args(P, D);
skip_args([], _) -> [].
