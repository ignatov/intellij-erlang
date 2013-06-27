-module(singleFunction).

-behaviour(test_behaviour).
-behaviour(test_behaviour2).

-export([test_callback/1]).

<caret>test_callback(_) -> ok.