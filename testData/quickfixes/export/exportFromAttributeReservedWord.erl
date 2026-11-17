-module(exportFromAttributeReservedWord).
-export([start/0<caret>]).

start() -> 'end'().
'end'() -> ok.
