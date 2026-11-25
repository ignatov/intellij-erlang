-module(x).
-compile({parse_transform, abig}).
-compile({parse_transform, zbig}).
-export([parse_transform/2]).

parse_transform(Forms, _Options) -> Forms.
