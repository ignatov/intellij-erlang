-module(macroWholeFunExport).

-define(FUN, foo() -> ok).

?FUN<caret>.