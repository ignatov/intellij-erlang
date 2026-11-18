-module(m).
-ifdef(TEST).
-define(name, x).
?na<caret>me() -> ok.
-endif.
