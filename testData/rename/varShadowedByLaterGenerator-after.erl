-module(varShadowedByLaterGenerator).
-export([f/1]).

f(L) -> [{Zz<caret>, Y} || X <- L, {Zz, Y} <- L].
