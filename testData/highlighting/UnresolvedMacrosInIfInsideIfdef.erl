-module('UnresolvedMacrosInIfInsideIfdef').
-export([f1/0, f2/0, g1/0, g2/0]).

-ifndef(M).
-<error descr="<attribute>, <behaviour>, callback, define, else, export, export_record, export_type, import, import_record, include, include_lib, module, nominal, on_load, opaque, optional_callbacks, record, spec or type expected, got 'if'">i</error>f(false).
f1() -> ok.
-else.
f2() -> <error descr="Unresolved macros '?M'">?M</error>.
-endif.
-endif.

-ifdef(N).
-<error descr="<attribute>, <behaviour>, callback, define, else, export, export_record, export_type, import, import_record, include, include_lib, module, nominal, on_load, opaque, optional_callbacks, record, spec or type expected, got 'if'">i</error>f(true).
g1() -> ?N.
-endif.
g2() -> ?N.
-endif.
