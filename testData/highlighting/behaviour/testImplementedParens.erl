-module(testImplementedParens).

-behaviour(bParens<caret>).

on_receive(Data) -> Data.
