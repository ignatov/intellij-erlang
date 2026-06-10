-module(nativeRecordUsagesInMultipleFiles).
-export([foo/0]).

-import_record(nativeRecordUsagesInSingleFile, [point]). % 4

foo() ->
  #point{}, % 5
  #nativeRecordUsagesInSingleFile:point<caret>{}. % 6
