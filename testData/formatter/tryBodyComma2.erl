test() ->
  try
    ok,
    fail(),<caret>
  catch _:_ -> error
  end.
