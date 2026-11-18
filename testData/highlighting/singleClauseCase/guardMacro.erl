-module(guardMacro).
-export([b/1]).

-define(G(X), X =:= a; X =:= b).

b(X) -> <weak_warning descr="'case' expression has only one clause">case</weak_warning> X of Y when ?G(Y) -> ok end.
