-module(testTupleOptional).

-behaviour(bTuple).

-export([on_receive/1]).

on_receive(Data) -> Data.
