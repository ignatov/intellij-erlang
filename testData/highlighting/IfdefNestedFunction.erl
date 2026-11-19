-export([run/0]).

-ifdef(A).
-ifdef(B).
h() -> 1.
-else.
h() -> 2.
-endif.
-elif(true).
h() -> 3.
-else.
h() -> 4.
-endif.

-ifdef(C).
<warning>k</warning>() -> 1.
-endif.
-ifdef(C).
<warning><warning>k</warning></warning>() -> 2.
-endif.

run() -> {h(), k()}.
