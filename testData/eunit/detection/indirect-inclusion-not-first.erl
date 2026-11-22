-module('indirect-inclusion-not-first').

-include("include-eunit-not-first.hrl").

foo_test() ->
  ?assert(true).
