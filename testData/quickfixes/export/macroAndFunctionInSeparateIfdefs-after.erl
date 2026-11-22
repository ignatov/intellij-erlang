-module(m).
-ifdef(TEST).
-export([?name/0]).

-define(name, x).
-endif.
-ifdef(TEST).
?na<caret>me() -> ok.
-endif.
