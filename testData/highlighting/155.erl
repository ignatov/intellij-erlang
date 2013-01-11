-export([keep_alive/2]).

on_exit(_Pid, _Fun) -> ok.

keep_alive(Name, Fun) ->
  register(Name, Pid = spawn(Fun)),
  on_exit(Pid, fun(_Why) -> keep_alive(Name, Fun) end).
