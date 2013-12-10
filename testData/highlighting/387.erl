-export([error/0, ok_2/0, ok_1/0]).

ok_1() ->
  Whole = [{Key = key, Value = value}] = [{key, value}],
  [Whole, Key, Value].

ok_2() ->
  [{Key = key, Value = value}],
  [Key, Value].

error() ->
  Whole = [{Key = key, Value = value}],
  [Whole, Key, Value].