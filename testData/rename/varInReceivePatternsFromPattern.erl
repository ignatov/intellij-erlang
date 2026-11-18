-module(varInReceivePatternsFromPattern).
-export([restarter/0]).

restarter() ->
  process_flag(trap_exit, true),
  Pid = spawn_link(?MODULE, critic2, []),
  register(critic, Pid),
  receive
    {'EXIT', Pid, normal} -> ok;
    {'EXIT', P<caret>id, shutdown} -> ok;
    {'EXIT', Pid, _} -> restarter()
  end.
