-module(m).
-ifdef(TEST).
-define(name, x).
-endif.
-ifdef(TEST).
?na<caret>me() -> ok.
-endif.
