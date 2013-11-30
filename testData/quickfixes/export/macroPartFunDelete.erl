-module(macroPartFunDelete).

-define(FUN, foo() -> ).

?FUN<caret> ok.