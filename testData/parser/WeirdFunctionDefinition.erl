-module('WeirdFunctionDefinition').

-export([foo/0]).

-define(F, foo).
-define(LPAR, ().
-define(RPAR, )).
-define(ARROW, ->).
-define(OK, ok).

?F ?LPAR ?RPAR ?ARROW ?OK.