-export([strict_list/0, strict_binary/0, strict_map/0, zip/0, zip_mixed/0]).

%% Strict and zip generators should report errors on SDK < 28

strict_list() ->
    [X || {ok, X} <error descr="Strict generators are only supported in Erlang 28 and newer versions"><:-</error> [{ok, 1}, {ok, 2}]].

strict_binary() ->
    [X || <<X>> <error descr="Strict generators are only supported in Erlang 28 and newer versions"><:=</error> <<1, 2, 3>>].

strict_map() ->
    [V || _ := V <error descr="Strict generators are only supported in Erlang 28 and newer versions"><:-</error> #{a => 1, b => 2}].

zip() ->
    [{X, Y} || X <- [1, 2] <error descr="Zip generators are only supported in Erlang 28 and newer versions">&&</error> Y <- [a, b]].

zip_mixed() ->
    [{X, Y} || X <error descr="Strict generators are only supported in Erlang 28 and newer versions"><:-</error> [1, 2, 3] <error descr="Zip generators are only supported in Erlang 28 and newer versions">&&</error> Y <- [a, b, c], X > 1].
