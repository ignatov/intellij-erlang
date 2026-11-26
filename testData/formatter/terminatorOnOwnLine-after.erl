-module(m).

foo() ->
  ok
.

bar(1) ->
  ok,
  ;
bar(_) ->
  ok,
  .

baz() ->
  ok, % more to come
  .
