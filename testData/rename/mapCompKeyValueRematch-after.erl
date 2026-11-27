-module(mapCompKeyValueRematch).
-export([f/1]).
f(_ = <caret>New) ->
    #{case ok of {<<_:New>>} -> ok end => try ok catch error:New -> ok end || ok}.
