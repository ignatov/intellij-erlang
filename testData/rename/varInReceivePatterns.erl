-module(varInReceivePatterns).
-export([restarter/0]).

restarter() ->
  process_flag(trap_exit, true),
  P<caret>id = spawn_link(?MODULE, critic2, []),
  register(critic, Pid),
  receive
    {'EXIT', Pid, normal} -> ok;
    {'EXIT', Pid, shutdown} -> ok;
    {'EXIT', Pid, _} -> restarter()
  end.
