-module(singleFunction).

-behaviour(test_behaviour).

-export([test_callback/1]).

<caret>test_callback(_) -> ok.