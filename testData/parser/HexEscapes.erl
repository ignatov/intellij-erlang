-module('HexEscapes').
-export([chars/0, atoms/0, string/0]).

chars() -> [$\x00, $\xFF, $\xaf, $\x{41}, $\x{10FFFF}, $\x{0041}].

atoms() -> ['\x41', '\x{41}', 'a\x42c'].

string() -> "\x{41}\x42".
