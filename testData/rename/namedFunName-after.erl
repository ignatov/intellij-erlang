-module(namedFunName).
-export([foo/0]).

foo() ->
  fun
    Fact
    (1, _) -> Fact(0, 0);
    Fact
    (0, NamedFun) ->
      NamedFun(1, 0);
    Fact(0, _) ->
      Fact(1, 1)
  end.
