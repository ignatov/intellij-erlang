-module('ControlEscapeAtStringEnd').
-export([string/0, concat/0, sigil/0, sigil_b/0, binary/0, after_strings/0]).

string() -> "\^\".

concat() -> "a\^\" ++ "b".

sigil() -> ~"\^\".

sigil_b() -> [~b"\^\", ~s"\^\"].

binary() -> <<"\^\">>.

after_strings() -> {ok, 'atom', "c"}.
