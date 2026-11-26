-export([f/2, g/2]).

f(X, Y) when ceil(X) > 1, floor(Y) < 5, max(X, Y) > 0, min(X, Y) < 9 -> ok.

g(X, Y) when erlang:ceil(X) > 1, erlang:floor(Y) < 5, erlang:max(X, Y) > 0, erlang:min(X, Y) < 9 -> ok.
