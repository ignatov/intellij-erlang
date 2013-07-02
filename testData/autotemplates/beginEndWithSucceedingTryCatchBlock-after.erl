foo() ->
  try something of
    _ -> ok, begin
               <caret>
             end
  catch
    _ -> ok
  end.