-module(singleClause).
-export([f/1]).

f(X) ->
  <weak_warning descr="'case' expression has only one clause">case</weak_warning> X of
    {ok, Y} -> Y
  end.
