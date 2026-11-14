-module(m).
-ifdef(TEST).
-export([?name/0]).

-define(name, test_name).
-else.
-define(name, prod_name).
-endif.

?na<caret>me() -> ok.
