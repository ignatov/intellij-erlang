-module(macroPartFunExport).

-define(FUN, foo() -> ).

?FUN<caret> ok.