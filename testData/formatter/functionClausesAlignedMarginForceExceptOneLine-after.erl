g(a)  -> ok;
g(bb) -> ok;
g(some_long_pattern) ->
  first_call(argument), second(x).

h(some_long_pattern_x) -> ok;
h(a)                   -> first(argument), second(arg).
