-module(testCreatedParens).

-behaviour(bParens).

-export([on_receive/1]).

on_receive(Data) ->
  erlang:error(not_implemented).