-module(test).
-export([g/0]).
-define(A, true).
-include("a.hrl").
-include("b.hrl").
-include("c.hrl").
-include("d.hrl").
-include("e.hrl").
-include("g.hrl").
-include("h.hrl").
-include("i.hrl").

g() -> <caret>f().
