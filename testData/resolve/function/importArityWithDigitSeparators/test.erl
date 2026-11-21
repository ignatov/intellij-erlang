-module(test).
-export([foo/0]).

-import(incl, [bar/0_1]).

foo() -> ba<caret>r(1).
