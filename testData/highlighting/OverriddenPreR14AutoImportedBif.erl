-module('OverriddenPreR14AutoImportedBif').
-export([f/0]).
f() -> length([]).
length(L) -> L.
