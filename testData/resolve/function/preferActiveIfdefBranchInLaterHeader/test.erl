-module(test).
-export([g/0]).
-include("a.hrl").
-include("b.hrl").

g() -> <caret>f().
