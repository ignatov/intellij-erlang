-module(native_records_remote).

-record #point{x = 0, y = 0}.
-record #hidden{secret = 0}.

-export_record([point]).
