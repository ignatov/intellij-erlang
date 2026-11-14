-module(verbatim_sigil_backslash).
-export([f/0, g/0, h/0]).

f() -> {~B"\", ~"x"}.
g() -> ~S"\".
h() -> ~B[\].
