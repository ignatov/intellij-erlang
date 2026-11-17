-module(reservedWordFun_target).

-export(['end'/1]).

bar() -> ok.

'end'(_Arg0) ->
  erlang:error(not_implemented).
