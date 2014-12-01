-module(debugnode).

-export([main/1]).

main([PortText]) ->
  {ok, Host} = inet:gethostname(),
  try erlang:list_to_integer(PortText) of
    Port -> connect_and_run(Host, Port)
  catch
    error:badarg -> io:format("~s~s~n", ["Invalid port: ", PortText])
  end.

connect_and_run(Host, Port) ->
  case gen_tcp:connect(Host, Port, [binary, {packet, 4}, {active, false}]) of
    {ok, Socket} -> run(Socket);
    {error, Reason} -> io:format("~s~n~p~n", ["Connection failed: ", Reason])
  end.

run(Socket) ->
  process_flag(trap_exit, true),
  {Debugger, _} = spawn_opt(remote_debugger, run, [Socket], [monitor, link]),
  spawn_opt(remote_debugger_notifier, run, [Debugger], [monitor, link]),
  spawn_opt(remote_debugger_listener, run, [Debugger], [monitor, link]),
  wait_for_exit().

wait_for_exit() ->
  receive
    {'EXIT', _, _} -> stop_debugger();
    {'DOWN', _, _, _, _} -> stop_debugger();
    _ ->
      wait_for_exit()
  end.

stop_debugger() ->
  int:stop(),
  ok.