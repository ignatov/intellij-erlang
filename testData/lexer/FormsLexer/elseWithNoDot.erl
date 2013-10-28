-module(elseWithNoDot).

-ifdef(D).
-else
-define(D, ok).
-endif.