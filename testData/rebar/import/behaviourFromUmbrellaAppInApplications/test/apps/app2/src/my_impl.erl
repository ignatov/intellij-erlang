-module(my_impl).
-behaviour(gen_my_behaviour).

-export([foo/0]).

foo() -> ok.
