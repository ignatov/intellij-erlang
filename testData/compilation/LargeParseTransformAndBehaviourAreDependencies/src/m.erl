-module(m).
-compile({parse_transform, pt}).
-behaviour(beh).
-export([init/0]).

init() -> ok.
