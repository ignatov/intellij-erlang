-module('VerbatimSigilControlEscape').
-export([s/0, b/0, paren/0, bracket/0, brace/0, concat/0, other/0, after_sigils/0]).

s() -> ~S"\^".

b() -> ~B"\^".

paren() -> ~S(\^).

bracket() -> ~B[\^].

brace() -> ~S{\^}.

concat() -> ~S"a\^" ++ "b".

other() -> [~S<\^>, ~B|\^|, ~S'\^', ~S`\^`, ~S#\^#, ~S/\^/].

after_sigils() -> {ok, 'atom', "c"}.
