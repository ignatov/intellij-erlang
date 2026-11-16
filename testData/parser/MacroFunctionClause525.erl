-module(macroFunctionClause525).
-export([do_handle_info/2]).

-define(MI, mi).
-define(MODULE_HANDLE_ONE(Module, HandleModule),
  do_handle_info({Unique, Module, Method, DataIn, ActorID, Pid, Port}, _State) ->
    HandleModule:handle({Unique, Module, Method, DataIn, ActorID, Pid, Port})).

do_handle_info({mod, Module, Msg}, _State) ->
  Module:handle(Msg);
?MODULE_HANDLE_ONE(?MI, mod_mi);
do_handle_info(_, _) ->
  ok.
