-module(a).
-export([f/1]).

f(L) when length(L) > 0 -> <caret>Length = length(L),
  {Length, Length}.
