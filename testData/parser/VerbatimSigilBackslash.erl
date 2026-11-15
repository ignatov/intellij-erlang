-module(verbatim_sigil_backslash).
-export([f/0, g/0, h/0, delimiters/0, others/0]).

f() -> {~B"\", ~"x"}.
g() -> ~S"\".
h() -> ~B[\].

delimiters() ->
    [~B'\', ~S(\), ~B{\}, ~S<\>, ~B/\/, ~S|\|, ~B`\`, ~S#\#].

others() ->
    {~B"..\..", ~B"\\", ~S"a\nb", ~b"\"", ~s(\)), ~"\"", ~B"""
    \
    """}.
