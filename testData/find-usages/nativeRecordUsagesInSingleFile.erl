-module(nativeRecordUsagesInSingleFile).
-export([local/0]).

-record #point<caret>{x = 0, y = 0}.
-export_record([point]). % 1

local() ->
  #point{}, % 2
  #point{x = 1}. % 3
