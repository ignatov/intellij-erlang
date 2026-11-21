foo(X) ->
  Sign = if
           X > 0 -> pos;
           true -> neg
         end,
  bar(Sign).

bar(R) -> R.
