-define(BASE, uid :: atom()).
-record(r, {?BASE}).
t(R) -> {#r{ui<caret>d = a}, id, {id, 1}, R#r.uid}.
