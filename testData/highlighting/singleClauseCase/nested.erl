-module(nested).
-export([f/1]).

f(X) ->
  case X of
    {ok, Y} ->
      <weak_warning descr="'case' expression has only one clause">case</weak_warning> Y of
        Z when is_integer(Z) -> Z
      end;
    _ -> error
  end.
