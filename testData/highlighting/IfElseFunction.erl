-export([run/0]).

-<error descr="<attribute>, <behaviour>, callback, define, else, export, export_record, export_type, import, import_record, include, include_lib, module, nominal, on_load, opaque, optional_callbacks, record, spec or type expected, got 'if'">i</error>f(?OTP_RELEASE >= 21).
f() -> 1.
-elif(true).
f() -> 2.
-else.
f() -> 3.
-endif.

run() -> f().
