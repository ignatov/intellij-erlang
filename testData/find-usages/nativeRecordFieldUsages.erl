-module(nativeRecordFieldUsages).
-export([foo/1]).

-record #point{x<caret> = 0, y = 0}.

foo(Point) ->
  Point#point.x, % 1
  #point{x = 1}, % 2
  Point#point{x = 2}. % 3
