-export([foo/0]).
-compile({no_auto_import, [abs /  1]}).

abs(I) -> I.
foo() -> abs(3).