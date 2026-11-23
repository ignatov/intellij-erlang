-module('ImportOverridesBifAutoImportedSinceR14').
-export([f/0]).
-import(x, [binary_part/2]).
f() -> binary_part(<<>>, {0, 0}).
