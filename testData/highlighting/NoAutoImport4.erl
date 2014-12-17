-export([foo/0]).
-compile([{inline,[pi/0]}, {no_auto_import, [abs /  1, foo/2]}]).

abs(I) -> I.
foo() -> abs(3).