-module('ControlEscapes').
-export([chars/0, atoms/0, string/0]).

chars() -> [$\^@, $\^_, $\^?, $\^a, $\^z, $\^A, $\^[, $\^\, $\^], $\^^].

atoms() -> ['\^?', '\^a', 'a\^@b'].

string() -> "\^?\^a".
