-define (get_int32(N), N:32/signed-little).
parse(<<?get_int32(Len), _/bits>>) ->
  io:format("Len: ~p", [Len]).