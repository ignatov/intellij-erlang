-module(macroPartFunExport).

-define(FUN, foo() -> ).

-export([foo/0]).

?FUN<caret> ok.