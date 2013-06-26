-export([f/0, f/1, f_macro/0]).

-record(tid, {kkk, id}).

-define(KKK_MACRO, kkk).

f() ->
    #tid{kkk=10, <error>kkk1</error>=1}.

f(A) ->
    A#tid.kkk, A#tid.<error>kkk1</error>.

f_macro() ->
    #tid{?KKK_MACRO=1, id=0}.