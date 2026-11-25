-export([f/1]).

f(L) ->
  lists:filter(fun({K, K}) -> true; (_) -> false end, L).
