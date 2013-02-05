-define(DICT, dict).
-define(SET, set).

-record(state, {name,
        strategy               :: strategy(),
        children = []          :: [child_rec()],
        dynamics               :: ?DICT() | ?SET(), % Compiler fails here
        intensity              :: non_neg_integer(),
        period                 :: pos_integer(),
        restarts = [],
            module,
            args}).