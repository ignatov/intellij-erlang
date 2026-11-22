-export([f/0, g/0]).
-export_type([t/0, u/0]).
-define(IGNORE(What), ok).
-define(C, ok).

-type t() :: ?IGNORE(fun F/1).
-type u() :: ?C(fun <error descr="A fun with a variable function name needs a module: fun M:F/Arity">F</error>/1).
-spec f() -> ?IGNORE(fun F/1).
f() -> ok.
-spec g() -> foo | ?IGNORE(fun F/1).
g() -> ok.
