-define (get_int32(N), N:32/signed-little).
parse(<<?get_int32(Len), _/bits>>) ->
  io:format("Len: ~p", [Len]).

-define(args(ARGS), ARGS + ARGS, ARGS,, ARGS,,,  ARGS).
-define(empty, ).
-define(commas, ,,,,,,).
-define(colons, ::::::).
-define(dots, .............).