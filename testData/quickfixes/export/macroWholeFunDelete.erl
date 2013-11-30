-module(macroWholeFunDelete).

-define(FUN, foo() -> ok).

?FUN<caret>.