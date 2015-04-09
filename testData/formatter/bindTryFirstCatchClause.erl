test() ->
  X = try
    fail()
  catch
    module:ex -> ok;<caret>