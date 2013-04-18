-export([bar/0]).
bar() ->
  io:format(1, 1, []),
  io:fwrite(<warning>"   "</warning>, [1]),
  io:format(<warning>"~p"</warning>, [1, 1]),
  io:format(1, <warning>""</warning>, [1]),
  io_lib:format(<warning>"~~"</warning>, [1]),
  io:format("~~~~"),
  io_lib:format("", 1),
  io_lib:format(<warning>"    f9_dmt_type:new(~p, ~p[]).~n~n"</warning>, [1]),
  io_lib:format("blah" "", []),
  io_lib:format(1, [1,2]),
  P = 1, Arg = 1, io_lib:format("~." ++ integer_to_list(P) ++ "g", [Arg]).