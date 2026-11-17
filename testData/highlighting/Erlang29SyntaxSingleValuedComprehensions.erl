-export([map_comprehension/0, map_comprehension_from_map/1, list_comprehension/0, tuple_list_comprehension/0,
         binary_comprehension/1]).

%% Single-valued comprehensions are valid on every OTP release and should not report
%% Erlang29SyntaxInspection errors (see #1093)

map_comprehension() ->
    #{ K => K+1 || K <- [1, 2, 3, 4, 5] }.

map_comprehension_from_map(Map) ->
    #{V => K || K := V <- Map}.

list_comprehension() ->
    [X * 2 || X <- [1, 2, 3, 4, 5]].

tuple_list_comprehension() ->
    [{X, Y} || X <- [1, 2], Y <- [a, b]].

binary_comprehension(Bin) ->
    << <<(X + 1)>> || <<X>> <= Bin >>.
