-module(conditionalExportAll_target).
-ifdef(TEST).
-compile(export_all).
-endif.
-export([bar/0]).

bar() -> ok.
