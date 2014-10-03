-module(expansionOccursInPlace).

-define(Y, 10).

-include("headers/defineXExpandingToCallToY.hrl").

ten() -> ?X.

-undef(Y).
-define(Y, 20).

twenty() -> ?X.