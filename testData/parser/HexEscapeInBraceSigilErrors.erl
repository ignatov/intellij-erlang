-module('HexEscapeInBraceSigilErrors').
-export([empty/0, not_hex/0, verbatim/0, after_sigils/0]).

empty() -> ~s{\x{}}.

not_hex() -> ~s{\x{zz}}.

verbatim() -> ~S{\x{41}}.

after_sigils() -> ok.
