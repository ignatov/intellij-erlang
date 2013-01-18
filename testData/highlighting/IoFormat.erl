-export([bar/0]).
bar() ->
  io:format(1, <warning>1</warning>, []),
  io:fwrite(<warning>"   "</warning>, [1]),
  io:format(<warning>"~p"</warning>, [1, 1]),
  io:format(1, <warning>""</warning>, [1]),
  io_lib:format(<warning>"~~"</warning>, [1]),
  io_lib:format("~~~~"),
  io_lib:format("", <warning>1</warning>),
  io_lib:format(<warning>1</warning>, [1,2]).