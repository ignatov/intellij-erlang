-export([foo/0]).
foo() ->
  maybe ok end.

-export([combat_may_begin/1]).

combat_may_begin(Players) ->
  maybe {minimum_player_count_reached} ?= minimum_player_check(Players),
  {players_are_ready} ?= player_readiness_check(Players),
  {combat_may_begin}
  end.

player_readiness_check(_) -> erlang:error(not_implemented).

minimum_player_check(_) -> erlang:error(not_implemented).