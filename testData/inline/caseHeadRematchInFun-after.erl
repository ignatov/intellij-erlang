foo() ->
  fun() ->
    case 1 = bar() of
      _ -> 1
    end
  end.

bar() -> 1.
