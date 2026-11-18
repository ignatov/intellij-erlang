-module(guardedSingleClause).
-export([f/1]).

f(X) ->
  <weak_warning descr="'case' expression has only one clause">case</weak_warning> X of
    Y when is_integer(Y) -> Y
  end.
