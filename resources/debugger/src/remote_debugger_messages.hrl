% Messages which can be received from remote debugger.

-record(interpret_modules, {modules = []}).
-record(run_debugger, {module, function, args = []}).
-record(debug_remote_node, {node, cookie = nocookie}).
-record(set_breakpoint, {module, line}).
-record(remove_breakpoint, {module, line}).
-record(step_into, {pid}).
-record(step_over, {pid}).
-record(step_out, {pid}).
-record(continue, {pid}).
-record(evaluate, {pid, expression}). % TODO consider adding a stack pointer as third param

% Messages which can be sent to remote debugger.
% Debugger implementation should handle all messages listed here.

-record(unknown_message, {msg}).
-record(register_listener, {pid}).
-record(interpret_modules_response, {node, statuses=[]}). % statuses is alist of pairs {module_name, ok|{error, reason}}
-record(set_breakpoint_response, {module, line, status}). % status=ok|{error, reason}
-record(evaluate_response, {result}).
-record(breakpoint_reached, {pid, snapshot}). % (see int:snapshot/0).
                                              % each snapshot has additional tuple element:
                                              % stack: [{SP,{Module, Function, ArgsList}, Bindings}] where bindings is [{atom(), term()}].
-record(debug_remote_node_response, {node, status}). % status=ok|{error, Reason}