-module(varShadowedByLaterGenerator).
-export([f/1]).

f(L) -> [{X<caret>, Y} || X <- L, {X, Y} <- L].
