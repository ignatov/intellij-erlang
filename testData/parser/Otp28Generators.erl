-module(otp28_generators).

%% Strict list generator (<:-)
strict_list() ->
    [X || {ok, X} <:- [{ok, 1}, error, {ok, 3}]].

%% Strict binary generator (<:=)
strict_binary() ->
    [X || <<X>> <:= <<1, 2, 3>>].

%% Strict map generator (<:-)
strict_map() ->
    [V || _ := V <:- #{a => 1, b => 2}].

%% Zip generator (&&) over lists
zip_lists() ->
    [{X, Y} || X <- [1, 2, 3] && Y <- [a, b, c]].

%% Zip generator mixing a strict and a relaxed generator plus a filter
zip_mixed() ->
    [{X, Y} || X <:- [1, 2, 3] && Y <- [a, b, c], X > 1].

%% Strict generator inside a binary comprehension
strict_binary_comprehension() ->
    << <<X>> || <<X>> <:= <<1, 2, 3>> >>.

%% Strict generator inside a map comprehension
strict_map_comprehension() ->
    #{K => V || K := V <:- #{a => 1, b => 2}}.

%% Zip generator inside a binary comprehension
zip_binary_comprehension() ->
    << <<(X + Y)>> || <<X>> <= <<1, 2>> && <<Y>> <= <<3, 4>> >>.
