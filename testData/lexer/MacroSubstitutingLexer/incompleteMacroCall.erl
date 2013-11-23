-module(incompleteMacroCall).

-define(MACRO, ok).
-define(RPAR, )).

%%%
%%% This code is not accepted by erlc - it tries to complete arguments list for ?MACRO call.
%%% Although it could be processed if ?MACRO was substituted as parameterless macro.
%%% Yet it's not, so we don't need to do the substitution here.
%%%
foo() -> ?MACRO( ?RPAR.