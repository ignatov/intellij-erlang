foo() ->
  try something of
    _ -> ok, begin<caret>
  catch
    _ -> ok
  end.