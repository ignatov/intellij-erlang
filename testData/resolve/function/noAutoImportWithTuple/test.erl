-module(test).
-export([foo/0]).

-compile({no_auto_import, [{is_atom, 1}]}).
-import(incl, [is_atom/1]).

foo() -> is_ato<caret>m(1).
