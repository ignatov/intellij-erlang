-module(m).
-export([other/0]).

other() -> ok.
'my-<caret>func'() -> ok.
