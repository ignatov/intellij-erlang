-module(remote_debugger_notifier).

-include("process_names.hrl").
-include("remote_debugger_messages.hrl").
-include("trace_utils.hrl").

-export([run/1, breakpoint_reached/1]).

run(Debugger) ->
  register(?RDEBUG_NOTIFIER, self()),
  attach_process(),
  loop(Debugger).

loop(Debugger) ->
  receive
    MessageToSend ->
      ?trace_message(MessageToSend),
      Debugger ! MessageToSend
  end,
  loop(Debugger).

attach_process() ->
  int:auto_attach([break], {?MODULE, breakpoint_reached, []}).

breakpoint_reached(Pid) ->
  ?RDEBUG_NOTIFIER ! #breakpoint_reached{pid = Pid, snapshot = snapshot_with_stacks()}.

snapshot_with_stacks() ->
  [{Pid, Init, Status, Info, get_stack(Pid, Status)} || {Pid, Init, Status, Info} <- int:snapshot()].

get_stack(Pid, break) ->
  do_get_stackframes(Pid);
get_stack(_, _) ->
  [].

do_get_stackframes(Pid) ->
  case dbg_iserver:safe_call({get_meta, Pid}) of
    {ok, MetaPid} ->
      Stack = int:meta(MetaPid, backtrace, all),
      [{SP, TraceElement, get_bindings(MetaPid, SP)} || {SP, TraceElement} <- Stack];
    Error ->
      io:format("Failed to obtain meta pid for ~p: ~p~n", [Pid, Error]),
      []
  end.

get_bindings(MetaPid, SP) ->
  int:meta(MetaPid, bindings, SP).