-define(BASE, i<caret>d :: atom()).
-record(r, {?BASE}).
t() -> #r{id = a}.
