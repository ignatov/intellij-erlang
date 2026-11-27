-module(test).
-export([g/0]).
-define(A, true).
-include("a.hrl").
-undef(A).
-include("b.hrl").

g() -> <caret>f().
