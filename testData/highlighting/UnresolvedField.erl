-export([f/0, f/1]).

-record(tid, {kkk, id}).

f() ->
    #tid{kkk=10, <error>kkk1</error>=1}.

f(A) ->
    A#tid.kkk, A#tid.<error>kkk1</error>.