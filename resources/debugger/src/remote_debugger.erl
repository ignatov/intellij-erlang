-module(remote_debugger).

-export([run/1]).

-include("remote_debugger_messages.hrl").

run(PassiveSocket) ->
  ListenerPid = receive #register_listener{pid = Pid} -> Pid end,
  spawn_link(fun() -> reader_loop(PassiveSocket, ListenerPid) end),
  writer_loop(PassiveSocket).

writer_loop(Socket) ->
  receive
    #register_listener{} -> ignore;
    Message ->
      Packet = erlang:term_to_binary(Message),
      gen_tcp:send(Socket, Packet)
  end,
  writer_loop(Socket).

reader_loop(Socket, Listener) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Packet} ->
      Message = erlang:binary_to_term(Packet),
      Listener ! Message;
    _ -> ignore
  end,
  reader_loop(Socket, Listener).