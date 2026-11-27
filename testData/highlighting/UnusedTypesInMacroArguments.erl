-export([main/0, main2/0]).
-define(LIST(X), [X]).
-define(OPT(T), T | undefined).
-define(T(X), X).
-record(r, {a :: ?OPT(<warning descr="Unresolved function mt2/0">mt2</warning>())}).
-type <warning descr="Unused type 'mt1'">mt1</warning>() :: via1().
-type via1() :: ok.
-type <warning descr="Unused type 'mt2'">mt2</warning>() :: via2().
-type via2() :: ok.
-type <warning descr="Unused type 'mt3'">mt3</warning>() :: via3().
-type via3() :: ok.
-type top() :: ?T(<warning descr="Unresolved function mt3/0">mt3</warning>()).
-spec main() -> ?LIST(<warning descr="Unresolved function mt1/0">mt1</warning>()).
main() -> ok.
-spec main2() -> top().
main2() -> #r{}.
