-module(unused_remote_type_macro).
-export([main/0]).
-define(M, unused_remote_type_macro).
-type <warning descr="Unused type 'mt'">mt</warning>() :: via().
-type via() :: ok.
-spec main() -> ?M:mt().
main() -> ok.
