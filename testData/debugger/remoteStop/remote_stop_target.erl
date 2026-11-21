-module(remote_stop_target).
-export([current/0]).

%% Returns {dbg_debugged, _, _} while the module is interpreted and {remote_stop_target, current, 0} otherwise.
current() -> element(2, process_info(self(), current_function)).
