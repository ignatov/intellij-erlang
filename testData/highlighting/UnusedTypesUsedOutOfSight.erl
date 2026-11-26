-export([main/0]).
-define(T, <warning descr="Unresolved function mt/0">mt</warning>()).
-type <warning descr="Unused type 'mt'">mt</warning>() :: via_mt().
-type via_mt() :: ok.
-spec main() -> ?T.
main() -> ok.
