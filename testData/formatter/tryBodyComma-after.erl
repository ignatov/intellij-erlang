test() ->
  try
    ok,
    <caret>
  catch _:_ -> error
  end.
