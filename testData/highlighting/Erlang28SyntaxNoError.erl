-export([strict_list/0, strict_binary/0, strict_map/0, zip/0, zip_mixed/0,
         based_floats/0, to_meters/1]).

%% OTP 28 syntax should not report Erlang28SyntaxInspection errors on OTP 28+

-nominal meter() :: integer().

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

based_floats() ->
    2#0.111,
    16#fefe.fefe#e16,
    16#1.8#e-1,
    2#0.1111_1111#E8,
    3.14.

-spec to_meters(meter()) -> meter().
to_meters(M) ->
    M.
