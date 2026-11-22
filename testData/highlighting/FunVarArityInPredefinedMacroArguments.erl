-export([a/0, b/1, c/0, d/0, e/0, f/0, g/0, h/0]).

a() -> ?MODULE(fun <error descr="A fun with a variable function name needs a module: fun M:F/Arity">X</error>/1).
b(_) -> ?MODULE:?FUNCTION_NAME(fun <error descr="A fun with a variable function name needs a module: fun M:F/Arity">X</error>/1).
c() -> ?LINE(fun <error descr="A fun with a variable function name needs a module: fun M:F/Arity">X</error>/1).
d() -> ?FILE(fun <error descr="A fun with a variable function name needs a module: fun M:F/Arity">X</error>/1).
e() -> ?MACHINE(fun <error descr="A fun with a variable function name needs a module: fun M:F/Arity">X</error>/1).
f() -> ?FUNCTION_ARITY(fun <error descr="A fun with a variable function name needs a module: fun M:F/Arity">X</error>/1).
g() -> ?OTP_RELEASE(fun <error descr="A fun with a variable function name needs a module: fun M:F/Arity">X</error>/1).
h() -> ?MODULE_STRING(fun <error descr="A fun with a variable function name needs a module: fun M:F/Arity">X</error>/1).
