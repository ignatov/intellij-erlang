-module(test).
-export([g/0]).
-include("a.hrl").
-include("b.hrl").
-include("c.hrl").

g() -> <caret>f().
