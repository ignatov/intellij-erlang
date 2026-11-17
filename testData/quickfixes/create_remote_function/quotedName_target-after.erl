-module(quotedName_target).

-export(['my-func'/1]).

bar() -> ok.

'my-func'(N) ->
  erlang:error(not_implemented).
