-define(DEFAULT_PRICE, 10).
-define(PRICE(Item), Item#item.price).

-record(item, {name :: atom(), price = ?DEFAULT_PRICE :: non_neg_integer()}).
