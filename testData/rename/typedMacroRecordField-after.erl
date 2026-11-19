-define(BASE, ui<caret>d :: atom()).
-record(r, {?BASE}).
t() -> #r{uid = a}.
