foo() ->
  fun() ->
    <caret>X = 1,
    case X = bar() of
      _ -> X
    end
  end.

bar() -> 1.
