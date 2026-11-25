-define(NAME, id).
-record(r, {?NAME = 1 :: integer()}).
t(R) -> {#r{i<caret>d = a}, id, {id, 1}, R#r.id}.
