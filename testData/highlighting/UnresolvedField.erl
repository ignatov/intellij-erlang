-export([f/0, f/1]).

-record(tid, {kkk, id}).

f() ->
    #tid{kkk=10, <warning>kkk1</warning>=1}.

f(A) ->
    A#tid.kkk, A#tid.<warning>kkk1</warning>.