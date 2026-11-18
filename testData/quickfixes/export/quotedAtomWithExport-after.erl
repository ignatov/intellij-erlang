-module(m).
-export([other/0, 'my-func'/0]).

other() -> ok.
'my-<caret>func'() -> ok.
