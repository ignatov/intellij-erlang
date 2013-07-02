foo() ->
  receive
    _ -> ok, begin
               <caret>
             end
  after
    10 -> ok
  end.