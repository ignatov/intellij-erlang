-module(x).

-export([perform/2, perform1/2, run/2, stsz_size/1]).

perform1(X, Y) ->
    <error>A</error>,
    Z = X ++ "X",
    [error_m || <warning>f</warning>(Z, <error>EbinProdDir</error>, Y), m:<warning>f</warning>(Z, m:<warning>v</warning>(Y), <error>EbinProdDir</error>)].

perform(X, Y) ->
    Z = X ++ "X",
    [error_m || <warning>f</warning>(Z, <error>EbinProdDir</error>, Y) ].

run(_, _) ->
    lists:<warning>map</warning>(fun(Spec) ->
        string:<warning>tokens</warning>(Spec, "@")
    end).

-spec x:stsz_size/1 :: (int) -> list.
stsz_size(SampleSizeData) ->
    lists:sum([S || <<S:32>> <= SampleSizeData]).