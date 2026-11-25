-module('DefinedOutsideIfElif').
-export([f/0]).
-if(lists:<warning descr="Unresolved function defined/1">defined</warning>(DEBUG)).
-define(X, 1).
-elif(<warning descr="Unresolved function defined/2">defined</warning>(DEBUG, 1)).
-define(X, 2).
-else.
-define(X, 3).
-endif.
f() -> <warning descr="Unresolved function defined/1">defined</warning>(?X).
