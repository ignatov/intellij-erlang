foo() ->
  receive
    _ -> ok, begin<caret>
  after
    10 -> ok
  end.