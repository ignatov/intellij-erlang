-module(shop_syntax).

%% erlc rejects this module: each function has a syntax error.

-export([total/1, kind/1]).

total(Items) ->
    lists:sum(Items) Items.

kind(Item) ->
    case Item of end.
