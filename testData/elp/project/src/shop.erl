-module(shop).

-include("shop.hrl").

-export([price/1, total/1, describe/1, log_price/1]).

-define(LOG(Value), io:format("~p~n", [Value])).

%% @doc Returns the price of the item named Name.
-spec price(shop_backend:key()) -> non_neg_integer().
price(Name) ->
  case shop_backend:lookup(shop_memory, Name) of
    {ok, Item} -> ?PRICE(Item);
    error -> ?DEFAULT_PRICE
  end.

-spec total([shop_backend:key()]) -> non_neg_integer().
total(Names) ->
  sum([price(Name) || Name <- Names], 0).

-spec describe(#item{}) -> string().
describe(#item{name = Name, price = Price}) ->
  lists:flatten(io_lib:format("~p costs ~p", [Name, Price])).

-spec log_price(shop_backend:key()) -> ok.
log_price(Name) ->
  ?LOG(price(Name)).

sum([], Acc) -> Acc;
sum([Price | Rest], Acc) -> sum(Rest, Acc + Price).
