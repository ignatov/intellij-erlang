-module(macroInsideMacroBody).

-define(MACRO1, ok).
-define(MACRO2, ?MACRO1).

foo() -> ?MACRO2.