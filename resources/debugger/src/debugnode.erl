-module(debugnode).

-export([main/1]).

main([TargetNodeName, TargetProcessName]) ->
  main(TargetNodeName, TargetProcessName).

main(TargetNodeName, TargetProcessName) ->
  case is_alive() of
    false ->
      io:format("Should be launched in node runtime system.~n"),
      exit(1);
    _ ->
      %%TODO check if node name contains host part
      {ok, Host} = inet:gethostname(),
      run({list_to_atom(TargetProcessName),
           list_to_atom(TargetNodeName ++ [$@ | Host])})
  end.

run(Debugger) ->
  process_flag(trap_exit, true),
  spawn_opt(remote_debugger_notifier, run, [Debugger], [monitor, link]),
  spawn_opt(remote_debugger_listener, run, [Debugger], [monitor, link]),
  wait_for_exit().

wait_for_exit() ->
  receive
    {'EXIT', _, _} -> ok;
    {'DOWN', _, _, _, _} -> ok;
    _ ->
      wait_for_exit()
  end.
