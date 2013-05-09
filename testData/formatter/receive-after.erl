fooz(_, _) ->
  receive
    X ->
      X
  after
    1000 ->
      test()
  end,
  receive
    XBBBBB ->
      test(),
      foo();
    XBBBBB ->
      test(),
      foo()
  end,
  receive
  after
    100 ->
      100
  end.