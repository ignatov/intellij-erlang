-module(macroWholeFunExport).

-define(FUN, foo() -> ok).

-export([foo/0]).

?FUN<caret>.