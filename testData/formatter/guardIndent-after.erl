f(A, B)
  when is_list(A), is_list(B);
  is_binary(A), is_binary(B) ->
    {A, B}.

g(A) when is_list(A);
  is_binary(A) ->
    ok.

h(X) ->
    case X of
        {ok, Y}
          when is_integer(Y) ->
            Y;
        _ -> X
    end.

i(X) ->
    if
        X > 0 -> pos;
        true -> neg
    end.
