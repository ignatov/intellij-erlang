-module(aaa).

f() ->
  fuuuuuuuuuuuun(fun
    (X) -> X;
    (Y) -> Y
  end).

-record(x, {
  a = b :: atom()
}).