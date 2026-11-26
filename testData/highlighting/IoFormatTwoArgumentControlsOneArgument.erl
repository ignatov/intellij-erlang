-export([bar/1]).
bar(T) -> io:format(<warning descr="Wrong number of arguments in format call, should be 2">"~W"</warning>, [T]), io:format(<warning descr="Wrong number of arguments in format call, should be 2">"~.16X"</warning>, [255]), io_lib:format(<warning descr="Wrong number of arguments in format call, should be 3">"~*.16x"</warning>, [10, 255]), io:format("~*.16tx", [10, 255, "0x"]).
