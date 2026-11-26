-module('no-eunit').

-include_lib("kernel/include/file.hrl").
-include_lib("stdlib/include/assert.hrl").

foo_test() ->
  ?assert(true).
