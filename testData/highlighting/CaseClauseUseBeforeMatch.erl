-export([f/1]).

f(A) ->
    case A of
        _ -> Y = <error descr="Variable 'X' is unbound">X</error>, X = A, {X, Y}
    end.
