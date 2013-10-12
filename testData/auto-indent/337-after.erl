foo() ->
  case ok of
    notok ->
      if
        <caret>
      end,
      if
        true -> notok
      end;
    ok -> ok
  end.
