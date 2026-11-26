-module('HexEscapesErrors').

bare() -> $\x.

one_digit() -> $\x4.

not_hex() -> $\xg.

empty_braces() -> $\x{}.

unclosed() -> $\x{41.

atom_one_digit() -> '\x4'.

atom_not_hex() -> '\x{g}'.
