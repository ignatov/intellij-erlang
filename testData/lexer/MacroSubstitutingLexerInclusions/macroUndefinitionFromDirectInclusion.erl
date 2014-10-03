-module(macroUndefinitionFromDirectInclusion).

-define(X, 10).

-include("headers/undefX.hrl").

foo() -> ?X.