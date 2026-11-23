-module(recordInReadOnlyHeader).
-export([f/0]).
-include("recordInReadOnlyHeader.hrl").

f() -> #rec{<caret>new_field = 100}.
