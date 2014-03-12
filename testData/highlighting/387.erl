-export([error/0, ok_3/0, ok_2/0, ok_1/0]).

-record(record, {a}).

ok_1() ->
  Whole = [{Key = key, Value = value}] = [{key, value}],
  [Whole, Key, Value].

ok_2() ->
  [{Key = key, Value = value}],
  [Key, Value].

ok_3() ->
  X = #record{a = 1},
  #record{a = 1 = A} = X,
  [1 = 1 = A] = [X#record.a],
  #{a := 1 = 1 = A} = #{a => X#record.a}.

error() ->
  Whole = [{Key = key, Value = value}],
  [Whole, Key, Value].