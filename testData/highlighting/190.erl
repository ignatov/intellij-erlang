-export([format/2]).

format(Format, Args) -> io:format(user, Format, Args).