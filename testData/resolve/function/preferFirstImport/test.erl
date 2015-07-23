-module(test).
-export([foo/0]).

-import(incl, [is_atom/1]).
%%Error - Function already imported. Resolving to first import.
-import(another_incl, [is_atom/1]).

foo() -> is_ato<caret>m(1).
