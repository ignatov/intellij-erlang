-module(main).
-export([main/0]).

main() ->
  foo(),
  io:format("hello").

foo() ->
  erlang:error(not_implemented),
  ok.
