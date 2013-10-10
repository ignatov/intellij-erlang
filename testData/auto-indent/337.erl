foo() ->
  case ok of
    notok ->
      if<caret>
      if
        true -> notok
      end;
    ok -> ok
  end.
