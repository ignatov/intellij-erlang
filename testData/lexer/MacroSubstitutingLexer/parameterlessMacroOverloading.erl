-module(parameterlessMacroOverloading).

-define(BAR, bar).
-define(BAR(), bar).

foo() -> ?BAR()(), ?BAR.

bar() -> ok.