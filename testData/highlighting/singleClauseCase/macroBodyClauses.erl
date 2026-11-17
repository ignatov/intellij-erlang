-module(macroBodyClauses).
-export([a/1, b/1, c/1]).

-define(REST, ok; _ -> error).
-define(DEFAULT, ; _ -> error).
-define(OK, ok).

a(X) -> case X of a -> ?REST end.
b(X) -> case X of a -> "s" ?DEFAULT end.
c(X) -> <weak_warning descr="'case' expression has only one clause">case</weak_warning> X of a -> ?OK end.
