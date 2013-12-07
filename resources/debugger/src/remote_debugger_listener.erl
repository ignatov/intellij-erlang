-module(remote_debugger_listener).

% receives commands from remote debugger

-export([run/1]).

-include("process_names.hrl").
-include("remote_debugger_messages.hrl").
-include("trace_utils.hrl").

-record(state, {interpreted_modules = []}).

run(Debugger) ->
  register(?RDEBUG_LISTENER, self()),
  Debugger ! #register_listener{pid = self()},
  loop(#state{}).

loop(State) ->
  NewState = receive
               Message ->
                 ?trace_message(Message),
                 handle_message(Message, State)
             end,
  loop(NewState).

handle_message(Message, State) ->
  UsesState = uses_state(Message),
  if
    UsesState -> process_message(Message, State);
    true -> process_message(Message), State
  end.

uses_state(#interpret_modules{}) -> true;
uses_state(#debug_remote_node{}) -> true;
uses_state(_Message)             -> false.

process_message({interpret_modules, NewModules},
                #state{interpreted_modules = Modules} = State) when is_list(NewModules) ->
  interpret_modules(NewModules),
  State#state{interpreted_modules = Modules ++ NewModules};
process_message({debug_remote_node, Node, Cookie}, #state{interpreted_modules = Modules} = State) ->
  debug_remote_node(Node, Cookie, Modules), State.

% commands from remote debugger
process_message({set_breakpoint, Module, Line}) when is_atom(Module),
                                                     is_integer(Line) ->
  set_breakpoint(Module, Line);
process_message({remove_breakpoint, Module, Line}) when is_atom(Module),
                                                        is_integer(Line) ->
  remove_breakpoint(Module, Line);
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
  interpret_modules_on_node(Modules, node()).

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
    {[ArgsList | _], _} ->
      ArgsList;
    _ ->
      error
  end.

debug_remote_node(Node, Cookie, Modules) ->
  NodeConnected = connect_to_remote_node(Node, Cookie),
  Status = if
    NodeConnected -> interpret_modules_on_node(Modules, Node);
    true -> failed_to_connect
  end,
  send_debug_remote_node_response(Node, Status).

send_debug_remote_node_response(Node, ok) ->
  ?RDEBUG_NOTIFIER ! #debug_remote_node_response{node = Node, status = ok};
send_debug_remote_node_response(Node, Error) ->
  ?RDEBUG_NOTIFIER ! #debug_remote_node_response{node = Node, status = {error, Error}}.

connect_to_remote_node(Node, nocookie) ->
  net_kernel:connect(Node);
connect_to_remote_node(Node, Cookie) ->
  erlang:set_cookie(Node, Cookie),
  net_kernel:connect(Node).

interpret_modules_on_node(Modules, Node) ->
  case rpc:call(Node, lists, map, [fun int:ni/1, Modules]) of
    {badrpc, _} = Error -> Error;
    IntNiResults ->
      send_interpret_modules_response(Node, lists:zip(Modules, IntNiResults)),
      ok
  end.

send_interpret_modules_response(Node, IntResults) ->
  Statuses = lists:map(fun
              ({Module, {module, _}}) -> {Module, ok};
              ({Module, error}) -> {Module, int:interpretable(Module)}
            end, IntResults),
  ?RDEBUG_NOTIFIER ! #interpret_modules_response{node = Node, statuses = Statuses}.