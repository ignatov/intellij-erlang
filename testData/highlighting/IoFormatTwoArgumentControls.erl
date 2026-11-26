-export([bar/1]).
bar(T) -> io:format("~W", [T, 9]), io:format("~.16X", [255, "0x"]), io_lib:format("~.16x", [255, "0x"]).
