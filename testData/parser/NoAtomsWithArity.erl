-module(a).

-compile({no_auto_import, [foo / 1]}).

-compile([
  {no_auto_import, [[{aa / 2}]]},
  {no_auto_import, abs / 1}
  , inline, {no_auto_import, [foo / 1]}]).

foo() -> [aa/1, bb/3, cc/3].