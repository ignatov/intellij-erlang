-export([foo/0]).
-compile([{no_auto_import, [abs /  3]}]).

abs(I) -> I.
foo() -> <error>abs</error>(3).