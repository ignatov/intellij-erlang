-export([f/1]).
f(A) -> A.
foo() -> f(10).
<warning>bar</warning>() -> foo().