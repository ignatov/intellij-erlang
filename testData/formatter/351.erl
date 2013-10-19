foo() ->
  %% if is not
  Y = if
    1 -> true;
    true -> false
      end,

  %%after clause is not
  Z = receive
        1 -> true;
        0 -> false
  after 15 -> timeout
      end.