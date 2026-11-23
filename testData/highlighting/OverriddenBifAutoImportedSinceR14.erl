-module('OverriddenBifAutoImportedSinceR14').
-export([f/0]).
f() -> error("x", []).
error(F, A) -> {F, A}.
