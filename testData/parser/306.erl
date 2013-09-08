-module(m).
-on_load(load_my_nifs/0).

load_my_nifs() ->
    NifPath = "",
    Info = info,
    erlang:load_nif(NifPath, Info).