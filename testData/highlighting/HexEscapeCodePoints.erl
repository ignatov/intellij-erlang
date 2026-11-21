-module('HexEscapeCodePoints').
-export([chars/0, atoms/0, strings/0, sigils/0]).

chars() -> [$\x{10FFFF}, $\x{D7FF}, $\x{E000}, $\x{FDD0}, $\x{1FFFE}, $\x{0010FFFF}, $\xFF].

atoms() -> ['\x{10FFFF}', '\x{E000}'].

strings() -> ["\x{1FFFE}", "\\x{110000}", "\^\x{110000}", """
  \x{110000}
  """, <<"\x{10FFFF}"/utf8>>].

sigils() -> [~"\x{10FFFF}", ~B"\x{110000}", ~S"\x{D800}", ~B(\x{FFFF}), ~"""
  \x{110000}
  """].
