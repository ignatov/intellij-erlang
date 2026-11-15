-module(varInReceivePatterns).
-export([restarter/0]).

restarter() ->
  process_flag(trap_exit, true),
  C<caret>hild = spawn_link(?MODULE, critic2, []),
  register(critic, Child),
  receive
    {'EXIT', Child, normal} -> ok;
    {'EXIT', Child, shutdown} -> ok;
    {'EXIT', Child, _} -> restarter()
  end.
