-module(calls).
-export([r/1, helper/0, other/0, handle/1]).
-record(st, {val, other}).
-define(CLAUSES(A, B), r(1) -> A; r(2) -> B).
-define(HANDLER(R), handle(R) -> R#st.val).
?CLAUSES(helper(), other()).
?HANDLER(S).
helper() -> ok.
other() -> #st{val = 1}.
