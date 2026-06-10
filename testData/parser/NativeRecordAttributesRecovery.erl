-module(native_record_attributes_recovery).

-export_record([geometry:point]).
-import_record(geometry, [geometry:point]).
-export_record(point).
-import_record(geometry, point).

ok() -> ok.
