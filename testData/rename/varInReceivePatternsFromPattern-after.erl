-module(varInReceivePatternsFromPattern).
-export([restarter/0]).

restarter() ->
  process_flag(trap_exit, true),
  Child = spawn_link(?MODULE, critic2, []),
  register(critic, Child),
  receive
    {'EXIT', Child, normal} -> ok;
    {'EXIT', C<caret>hild, shutdown} -> ok;
    {'EXIT', Child, _} -> restarter()
  end.
