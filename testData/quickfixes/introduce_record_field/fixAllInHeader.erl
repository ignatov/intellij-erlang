-module(fixAllInHeader).
-export([f/0]).
-include("fixAllInHeader.hrl").

f() -> #rec{<caret>a = 1, b = 2}.
