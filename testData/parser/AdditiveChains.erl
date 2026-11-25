-module(additiveChains).
-export([f/0, g/0, h/1]).
f() -> 1 - 2 - 3.
g() -> 1 + 2 * 3 + 4.
h(X) -> (X + 1) - X bor 2.
