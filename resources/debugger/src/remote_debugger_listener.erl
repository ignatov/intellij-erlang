-module(remote_debugger_listener).

% receives commands from remote debugger

-export([run/1, interpret_module/1]).

-include("process_names.hrl").
-include("remote_debugger_messages.hrl").
-include("trace_utils.hrl").

run(Debugger) ->
  register(?RDEBUG_LISTENER, self()),
  Debugger ! #register_listener{pid = self()},
  loop().

loop() ->
  receive
    Message ->
      ?trace_message(Message),
      process_message(Message)
  end,
  loop().

% commands from remote debugger
process_message({set_breakpoint, Module, Line}) when is_atom(Module),
                                                     is_integer(Line) ->
  set_breakpoint(Module, Line);
process_message({remove_breakpoint, Module, Line}) when is_atom(Module),
                                                        is_integer(Line) ->
  remove_breakpoint(Module, Line);
process_message({interpret_modules, Modules}) when is_list(Modules) ->
  interpret_modules(Modules);
process_message({run_debugger, Module, Function, Args}) when is_atom(Module),
                                                             is_atom(Function),
                                                             is_list(Args) ->
  run_debugger(Module, Function, Args);
process_message({step_into, Pid}) when is_pid(Pid) ->
  step_into(Pid);
process_message({step_over, Pid}) when is_pid(Pid) ->
  step_over(Pid);
process_message({step_out, Pid}) when is_pid(Pid) ->
  step_out(Pid);
process_message({continue, Pid}) when is_pid(Pid) ->
  continue(Pid);
process_message({evaluate, Pid, Expression}) when is_pid(Pid),
                                                  is_list(Expression) ->
  evaluate(Pid, Expression);
% responses from interpreter
process_message({_Meta, {eval_rsp, EvalResponse}}) ->
  evaluate_response(EvalResponse);
% other
process_message({'DOWN', _, _, _, _}) ->
  exit(normal); % this means the process being debugged has quit
process_message(UnknownMessage) ->
  io:format("unknown message: ~p", [UnknownMessage]).

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

evaluate(Pid, Expression) ->
  {ok, Meta} = dbg_iserver:call({get_meta, Pid}),
  int:meta(Meta, eval, {undefined, Expression}). % Current module should be passed instead of 'undefined'

evaluate_response(Response) ->
  ?RDEBUG_NOTIFIER ! #evaluate_response{result = Response}.

parse_args(ArgsString) ->
  case erl_scan:string(ArgsString ++ ".") of
    {ok, Tokens, _} ->
      case erl_parse:parse_exprs(Tokens) of
        {ok, ExprList} ->
          eval_argslist(ExprList);
        _ ->
          error
      end;
    _ ->
      error
  end.

eval_argslist(ExprList) ->
  case erl_eval:expr_list(ExprList, erl_eval:new_bindings()) of
    {[ArgsList|_], _} ->
      ArgsList;
    _ ->
      error
  end.