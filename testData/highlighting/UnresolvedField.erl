-export([f/0]).

-record(tid, {kkk, id}).

f() ->
    #tid{kkk=10, <warning>kkk1</warning>=1}.