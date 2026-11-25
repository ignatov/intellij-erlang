h(X) ->
    case X of
        {ok, Y}
          when is_integer(Y) -><caret>
    end.
