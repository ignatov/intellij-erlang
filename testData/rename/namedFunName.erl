-module(namedFunName).
-export([foo/0]).

foo() ->
  fun
    NamedFun
    (1, _) -> NamedFun(0, 0);
    NamedFun
    (0, NamedFun) ->
      NamedFun(1, 0);
    NamedFun(0, _) ->
      <caret>NamedFun(1, 1)
  end.
