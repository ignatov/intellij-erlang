-module(reservedWordName_target).
-export([bar/0, 'if'/1]).

bar() -> ok.

'if'(N) ->
  erlang:error(not_implemented).
