-module(x).

-export([perform/2, perform1/2, run/2, stsz_size/1]).

perform1(X, Y) ->
    <warning>A</warning>,
    Z = X ++ "X",
    [error_m || f(Z, <warning>EbinProdDir</warning>, Y), m:f(Z, m:v(Y), <warning>EbinProdDir</warning>)].

perform(X, Y) ->
    Z = X ++ "X",
    [error_m || f(Z, <warning>EbinProdDir</warning>, Y) ].

run(_, _) ->
    lists:map(fun(Spec) ->
        string:tokens(Spec, "@")
    end).

stsz_size(SampleSizeData) ->
    lists:sum([S || <<S:32>> <= SampleSizeData]).