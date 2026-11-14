-module(m).
-export([other/0]).
-ifdef(TEST).
-define(name, test_name).
-else.
-define(name, prod_name).
-export([other/1]).
-endif.
-export([?name/0]).

other() -> ok.
other(_) -> ok.
?na<caret>me() -> ok.
