-module('UnresolvedMacrosInConditionalBranches').
-export([a1/0, a2/0, b1/0, b2/0, c1/0, c2/0, c3/0, e/0, f/0]).

-ifdef(A).
a1() -> ?A.
-else.
a2() -> <error descr="Unresolved macros '?A'">?A</error>.
-endif.

-ifndef(B).
b1() -> <error descr="Unresolved macros '?B'">?B</error>.
-else.
b2() -> ?B.
-endif.

-ifdef(C).
-ifdef(D).
c1() -> {?C, ?D}.
-else.
c2() -> {?C, <error descr="Unresolved macros '?D'">?D</error>}.
-endif.
c3() -> {?C, <error descr="Unresolved macros '?D'">?D</error>}.
-endif.

-ifdef(E).
-undef(E).
e() -> <error descr="Unresolved macros '?E'">?E</error>.
-endif.

f() -> <error descr="Unresolved macros '?A'">?A</error>.
