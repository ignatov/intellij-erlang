-module('BasedIntegers').
-export([f/0]).

f() ->
  [2#1010, 2#1_0, 8#17, 10#99, 16#ff_FF, 36#Zz, 36#Z_z, 1_6#F, 016#F, 0_2#1, 3_6#Z].
