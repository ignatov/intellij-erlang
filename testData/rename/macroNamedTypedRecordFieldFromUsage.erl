-define(NAME, id).
-define(BASE, ?NAME = 1 :: integer(), o :: atom()).
-record(r, {?BASE}).
t(R) -> {#r{i<caret>d = a}, id, {id, 1}, R#r.id}.
