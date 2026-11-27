-module(test).
-export([g/0]).
-include("def.hrl").
-include("a.hrl").
-include("b.hrl").

g() -> <caret>f().
