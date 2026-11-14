-module(testParens).

-behaviour(bParens).

-export([ready/1, on_receive/1]).

ready(Args) -> {ok, Args}.

on_receive(Data) -> Data.
