-module(macroInCatchPattern840).
-export([f/1]).

-ifdef('FUN_STACKTRACE').
-define(CAPTURE_STACKTRACE, ).
-define(GET_STACKTRACE, erlang:get_stacktrace()).
-else.
-define(CAPTURE_STACKTRACE, :__StackTrace).
-define(GET_STACKTRACE, __StackTrace).
-endif.

f(F) ->
  try F()
  catch Class:Reason ?CAPTURE_STACKTRACE -> {Class, Reason, ?GET_STACKTRACE}
  end.
