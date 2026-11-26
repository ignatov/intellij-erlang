foo(X) ->
  try case X of
        <caret>
      end catch _:_ -> ok end.