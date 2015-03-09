-module('IdMacroSelfApplication').

-define(ID(X), X).

foo ?ID(?ID(())) -> 11.