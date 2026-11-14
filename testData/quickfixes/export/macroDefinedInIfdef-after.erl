-module(m).
-ifdef(TEST).
-define(name, test_name).
-else.
-define(name, prod_name).
-endif.
-export([?name/0]).

?na<caret>me() -> ok.
