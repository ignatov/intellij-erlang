-module(exportFromAttributeReservedWord).
-export([start/0, 'end'/0]).

start() -> 'end'().
'end'() -> ok.
