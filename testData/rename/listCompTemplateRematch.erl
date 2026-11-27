-module(listCompTemplateRematch).
-export([f/1]).
f(_ = <caret>N) ->
    [{case ok of {<<_:N>>} -> ok end, try ok catch error:N -> ok end} || ok].
