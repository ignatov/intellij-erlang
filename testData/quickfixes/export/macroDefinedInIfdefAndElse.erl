-module(m).
-ifdef(TEST).
-define(name, test_name).
-else.
-define(name, prod_name).
-endif.

?na<caret>me() -> ok.
