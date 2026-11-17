-module(m).
-ifdef(TEST).
-export([?name/0]).

-define(name, x).
?na<caret>me() -> ok.
-endif.
