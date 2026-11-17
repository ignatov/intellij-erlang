test() ->
  try ok,
      b(),<caret>
  catch _:_ -> error
  end.
