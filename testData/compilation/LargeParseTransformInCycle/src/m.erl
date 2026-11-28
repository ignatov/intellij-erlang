-module(m).
-compile({parse_transform, pt}).
-export([parse_transform/2]).

parse_transform(Forms, _Options) -> Forms.
