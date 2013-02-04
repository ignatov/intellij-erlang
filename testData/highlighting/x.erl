-module(x).

-export([perform/2, perform1/2, run/2, stsz_size/1]).

perform1(X, Y) ->
    <error>A</error>,
    Z = X ++ "X",
    [error_m || <error>f</error>(Z, <error>EbinProdDir</error>, Y), m:<error>f</error>(Z, m:v(Y), <error>EbinProdDir</error>)].

perform(X, Y) ->
    Z = X ++ "X",
    [error_m || <error>f</error>(Z, <error>EbinProdDir</error>, Y) ].

run(_, _) ->
    lists:<error>map</error>(fun(Spec) ->
        string:tokens(Spec, "@")
    end).

stsz_size(SampleSizeData) ->
    lists:sum([S || <<S:32>> <= SampleSizeData]).