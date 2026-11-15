f(Y, L) ->
  SomeLongVar = fun() ->
                  Y
                end,
  lists:map(fun(X) ->
              X + 1
            end, L),
  F = fun(a) ->
        Y;
        (b) ->
          L
      end,
  {SomeLongVar, F}.
