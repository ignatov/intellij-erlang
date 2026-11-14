-module(testImplementedParens).

-behaviour(bParens).

-export([on_receive/1]).

on_receive(Data) -> Data.
