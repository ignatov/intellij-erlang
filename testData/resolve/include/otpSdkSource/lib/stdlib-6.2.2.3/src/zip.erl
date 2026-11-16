-module(zip).
-export([f/0]).

-include("zip.hrl").

f() -> #zip_file{name = a}.
