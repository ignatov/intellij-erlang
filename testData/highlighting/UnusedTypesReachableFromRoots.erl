-export([main/0]).
-export_type([exported/0]).
-record(r, {a :: in_record()}).
-callback cb(in_callback()) -> ok.

-type exported() :: from_exported().
-type from_exported() :: ok.
-type in_record() :: atom().
-type in_callback() :: atom().
-type in_unused_spec() :: from_unused_spec().
-type from_unused_spec() :: ok.
-opaque <warning descr="Unused type 'o1'">o1</warning>() :: o2().
-opaque <warning descr="Unused type 'o2'">o2</warning>() :: o1().
-type <warning descr="Unused type 't'">t</warning>(A) :: [A].
-type <warning descr="Unused type 't'">t</warning>() :: t(ok).

-spec unused() -> in_unused_spec().
<warning descr="Unused function 'unused/0'">unused</warning>() -> ok.
main() -> ok.
