-module('IoFormatSigils').
-export([deprecated/2, checked/0, skipped/0]).

deprecated(Mod, NewName) ->
  io_lib:format(~"use ~w:~w/~w", [Mod, NewName, 1]).

checked() ->
  io:format(<warning>"""
    ~p ~p
    """</warning>, [a]),
  io:format(<warning>~s"~p ~p"</warning>, [a]),
  io:format(<warning>~S"~p ~p"</warning>, [a]),
  io:format(<warning>~s(~p ~p)</warning>, [a]),
  io:format(<warning>~s"""
    ~p ~p
    """</warning>, [a]),
  io:format(<warning>~s"~~"</warning>, [a]),
  io:format(~s"<warning>~q</warning>", [a]),
  io:format(~s"~p ~p", [a, b]),
  io:format(~S"~10.3.\np", []),
  io:format(<warning>~S"~10.3.\np"</warning>, [a]),
  io:format(~S(~10.3.\np), []),
  io:format(~s"~10.3.\np", [a]).

skipped() ->
  io:format(~"~p ~p", [a]),
  io:format(~b"~p ~p", [a]),
  io:format(~B"~p ~p", [a]),
  io:format(~"""
    ~p ~p
    """, [a]),
  io:format(~"~q", [a]).
