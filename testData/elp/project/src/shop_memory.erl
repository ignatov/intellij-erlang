-module(shop_memory).
-behaviour(shop_backend).

-include("shop.hrl").

-export([fetch/1, items/0]).

-spec fetch(shop_backend:key()) -> {ok, #item{}} | error.
fetch(Key) ->
  case lists:keyfind(Key, #item.name, items()) of
    false -> error;
    Item -> {ok, Item}
  end.

-spec items() -> [#item{}].
items() ->
  [#item{name = apple, price = 3}, #item{name = pear}].
