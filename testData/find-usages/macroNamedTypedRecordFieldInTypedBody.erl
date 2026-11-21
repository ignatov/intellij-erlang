-define(NAME, id).
-define(BASE, ?NAME, x :: atom()).
-record(r, {?BASE}).
t(R) -> {#r{i<caret>d = a}, id, {id, 1}, R#r.id}.
