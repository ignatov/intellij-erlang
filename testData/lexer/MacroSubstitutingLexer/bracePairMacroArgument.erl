-module(bracePairMacroArgument).

-export([foo/0]).

-define(ID(X), X).

foo?ID(()) -> ok.