-module(predefinedMacrosInDirectInclusion).

-include("headers/predefinedMacros.hrl").

declaredFunction() ->
  ?USE_PREDEFINED_MACROS.