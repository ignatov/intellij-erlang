-module(own).
-define(N, 3).
-type priv() :: integer().
-type range() :: ?N..?N + 1.
-type bin() :: <<_:?N>>.
-spec f(?MODULE:priv()) -> ?MODULE:priv() | ?N.
f(X) -> X.
