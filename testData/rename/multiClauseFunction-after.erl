-module(multiClauseFunction).
-export(['BAR'/1]).

-spec 'BAR'(integer()) -> ok.
'B<caret>AR'(0) -> ok;
'BAR'(N) when N > 0 ->
  'BAR'(N - 1);
'BAR'(N) ->
  multiClauseFunction:'BAR'(-N).

foo() -> 'BAR'(1).
