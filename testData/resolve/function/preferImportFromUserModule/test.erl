-module(test).
-export([foo/0]).

%%Error - Import directive overrides pre R14 auto-imported BIF. Resolving to user code.
-import(incl, [is_atom/1]).

foo() -> is_ato<caret>m(1).
