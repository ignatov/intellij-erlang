-module(recordInHeader).
-export([f/0]).
-include("recordInHeader.hrl").

f() -> #rec{<caret>new_field = 100}.
