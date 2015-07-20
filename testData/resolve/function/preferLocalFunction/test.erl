-module(test).
-export([foo/0]).

%%Error - Import directive overrides pre R14 auto-imported BIF. Resolving to this module.
-import(incl, [is_atom/1]).

is_atom(A) -> A.

foo() -> is_ato<caret>m(1).
