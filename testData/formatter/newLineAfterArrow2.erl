one_line(x) -> ok, ok.

two_lines_1() -> ok,
  ok.

two_lines_2() ->
  ok,
  ok.

case_() -> case undef of
             one_line -> ok;
             two_lines_1 -> ok,
               ok;
             two_lines_2 ->
               ok,
               ok
           end.