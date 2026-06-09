-export([strict_list/0, strict_binary/0, strict_map/0, zip/0, zip_mixed/0]).

%% Strict and zip generators should not report Erlang28SyntaxInspection errors on OTP 28+

strict_list() ->
    [X || {ok, X} <:- [{ok, 1}, {ok, 2}]].

strict_binary() ->
    [X || <<X>> <:= <<1, 2, 3>>].

strict_map() ->
    [V || _ := V <:- #{a => 1, b => 2}].

zip() ->
    [{X, Y} || X <- [1, 2] && Y <- [a, b]].

zip_mixed() ->
    [{X, Y} || X <:- [1, 2, 3] && Y <- [a, b, c], X > 1].
