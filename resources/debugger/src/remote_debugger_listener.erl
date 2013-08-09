-module(remote_debugger_listener).

% receives commands from remote debugger

-export([run/1, interpret_module/1]).

-include("process_names.hrl").
-include("remote_debugger_messages.hrl").

run(Debugger) ->
  register(?RDEBUG_LISTENER, self()),
  Debugger ! #register_listener{pid = self()},
  loop().

loop() ->
  receive
    {set_breakpoint, Module, Line} when is_atom(Module),
                                        is_integer(Line) ->
      set_breakpoint(Module, Line);
    {remove_breakpoint, Module, Line} when is_atom(Module),
                                           is_integer(Line) ->
      remove_breakpoint(Module, Line);
    {interpret_modules, Modules} when is_list(Modules) ->
      interpret_modules(Modules);
    {run_debugger, Module, Function, Args} when is_atom(Module),
                                                is_atom(Function),
                                                is_list(Args) ->
      run_debugger(Module, Function, Args);
    {step_into, Pid} when is_pid(Pid) ->
      step_into(Pid);
    {step_over, Pid} when is_pid(Pid) ->
      step_over(Pid);
    {step_out, Pid} when is_pid(Pid) ->
      step_out(Pid);
    {continue, Pid} when is_pid(Pid) ->
      continue(Pid);
    {'DOWN', _, _, _, _} -> exit(normal); % this means the process being debugged has quit
    UnknownMessage ->
      io:format("unknown message: ~p", [UnknownMessage])
  end,
  loop().

set_breakpoint(Module, Line) ->
  Response = #set_breakpoint_response{
    module = Module,
    line = Line,
    status = int:break(Module, Line)
  },
  ?RDEBUG_NOTIFIER ! Response.

remove_breakpoint(Module, Line) ->
  int:delete_break(Module, Line).

interpret_modules(Modules) ->
  Statuses = lists:map(fun ?MODULE:interpret_module/1, Modules),
  ?RDEBUG_NOTIFIER ! #interpret_modules_response{statuses = Statuses}.

interpret_module(Module) ->
  case int:ni(Module) of
    {module, _} -> {Module, ok};
    error -> {Module, int:interpretable(Module)}
  end.

%%TODO handle all processes which are being debugged, not only the spawned one.
run_debugger(Module, Function, ArgsString) ->
  case parse_args(ArgsString) of
    error ->
      %%TODO report error
      exit(normal);
    ArgsList ->
      spawn_opt(Module, Function, ArgsList, [monitor, link])
  end.

step_into(Pid) ->
  int:step(Pid).

step_over(Pid) ->
  int:next(Pid).

step_out(Pid) ->
  int:finish(Pid).

continue(Pid) ->
  int:continue(Pid).

parse_args(ArgsString) ->
  case erl_scan:string(ArgsString ++ ".") of
    {ok, Tokens, _} ->
      case erl_parse:parse_term(Tokens) of
        {ok, ArgsList} when is_list(ArgsList) ->
          ArgsList;
        _ ->
          error
      end;
    _ ->
      error
  end.