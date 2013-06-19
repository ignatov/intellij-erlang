foo(AAAAAA) -> ok;
foo(A) ->
  tets,
  foo(1),
  ok;
foo(AAAAAAAAA) ->
  ok.

bar([])   -> empty;
bar(List) -> not_empty;