-module(testSeparatorsOptional).

-behaviour(bSeparators).

-export([on_receive/1]).

on_receive(Data) -> Data.
