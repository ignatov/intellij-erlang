-module('IfElifDefinedMacro').
-export([f/0]).
-if(defined(DEBUG_A) orelse defined(DEBUG_B)).
-define(X, 1).
-elif(defined(DEBUG_C)).
-define(X, 3).
-else.
-define(X, 2).
-endif.
f() -> ?X.
