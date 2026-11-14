-export([mkeysort/2, sort_by/2, shadow/1]).

mkeysort(K, L) ->
  lists:sort(
    fun(#{K := VA}, #{K := VB}) ->
      VA =< VB
    end,
    L
  ).

sort_by(Map, ListOfMaps) ->
  #{sort := Key} = Map,
  lists:sort(
    fun(#{Key := A}, #{Key := B}) ->
      A =< B
    end,
    ListOfMaps
  ).

shadow(<warning>K</warning>) ->
  fun(#{key := K}) -> K end.
