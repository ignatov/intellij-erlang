-module(native_records).

-record #vec{x = 0.0, y = 0.0}.
-record #empty{}.
-record #'quoted-record'{field = default :: atom()}.

-export_record([vec, empty, 'quoted-record']).
-import_record(geometry, [point, segment]).
-import_record('quoted-module', [remote]).

origin() ->
    #vec{x = 0.0, y = 0.0},
    #geometry:point{x = 0.0, y = 0.0}.
