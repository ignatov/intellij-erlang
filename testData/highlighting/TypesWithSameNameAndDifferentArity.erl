-export([f/0]).
-type tree() :: tree(term()).
-type tree(A) :: {node, A, [tree(A)]} | nil.
-spec f() -> tree().
f() -> nil.
