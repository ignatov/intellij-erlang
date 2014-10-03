-module(conditionallyDisabledInclusions).

-define(MACRO, 1).
-ifndef(MACRO).
  -include("headers/defineX.hrl").
-endif.
unresolved_macro_call() -> ?X.

-define(X, 0).
-ifndef(MACRO).
  -include("headers/undefX.hrl").
-endif.
resolved_macro_call() -> ?X.