-export([f/0, g/0, h/0, k/0]).
-define(F, foo).
-define(O, foo).
-define(O(A), ok).

foo(_) -> ok.

f() -> ?F(fun <error descr="A fun with a variable function name needs a module: fun M:F/Arity">X</error>/1).
g() -> lists:?F(fun <error descr="A fun with a variable function name needs a module: fun M:F/Arity">X</error>/1).
h() -> ?O(fun X/1).
k() -> foo(?O(fun X/1)).
