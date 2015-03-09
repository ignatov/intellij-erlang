-module('MacroCallSequence').

-define(L1, ?L2).
-define(L2, 1).


get_one() -> ?L1.