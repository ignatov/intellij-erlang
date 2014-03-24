multi_literal_string_first() ->
  "asdfasdf"
  "asdfasdfa"
  "asdfasdfa" ++
    X.

mix_l_and_r_associative_operators() ->
  X = 1 +
      2 +
      3 ++
      "something" = 94,
  C =
    b !
    b ! b !
    10 +
    189.

sample_from_issue_419() ->
  Code3 = "    performance_encode_test(10000, Record, {0, 0, 0, 0, 0}),\n"
          "    performance_decode_test(10000, " ++ Prefix ++ "_" ++ PName ++ ":encode(Record), {0, 0, 0, 0, 0}).\n\n"
          "performance_encode_test(0, _Record, {Cnt, TotalTime, TotalMemory, TotalHeap, TotalStack}) ->\n"
          "    io:format(\"Encode Performance Results for " ++ PName ++ "~n\"),\n".