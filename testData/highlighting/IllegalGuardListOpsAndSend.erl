-module('IllegalGuardListOpsAndSend').
-export([append/1, subtract/1, send/1, plus/1]).

append(L) when erlang:<error descr="Illegal guard expression">'++'(L, L)</error> =:= [] -> L.
subtract(L) when erlang:<error descr="Illegal guard expression">'--'(L, L)</error> =:= [] -> L.
send(L) when erlang:<error descr="Illegal guard expression">'!'(L, L)</error> =:= [] -> L.
plus(L) when erlang:'+'(L, L) =:= 0 -> L.
