f() ->
  case f(Sock<caret>et = g()) of
    ok -> Socket;
    _ -> {error, Socket}
  end,
  Socket.
