-module(macroDefinitionsSpanAcrossInclusions).

-define(define_x, 1).

-include("headers/condX.hrl").

foo() -> ?X.