-module(nestedIfElseInInactiveBranch).

-ifdef(inactive_branch).

-ifdef(whatever).
  ?COMMENTED_OUT.
-else.
  ?COMMENTED_OUT.
-endif.

-endif.