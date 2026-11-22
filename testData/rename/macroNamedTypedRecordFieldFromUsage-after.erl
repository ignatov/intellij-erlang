-define(NAME, uid).
-define(BASE, ?NAME = 1 :: integer(), o :: atom()).
-record(r, {?BASE}).
t(R) -> {#r{ui<caret>d = a}, id, {id, 1}, R#r.uid}.
