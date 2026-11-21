-module('HexEscapeInBraceSigil').
-export([s/0, b/0, default/0, two/0, concat/0, after_sigils/0]).

s() -> ~s{\x{41}}.

b() -> ~b{\x{41}}.

default() -> ~{\x{41}}.

two() -> ~s{\x{41}\x{42}}.

concat() -> ~s{a\x{41}b} ++ "c".

after_sigils() -> {ok, 'atom', "c"}.
