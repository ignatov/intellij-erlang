%%%-------------------------------------------------------------------
%%% @author ignatov
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Sep 2026 22:46
%%%-------------------------------------------------------------------
-module(main).
-author("ignatov").

%% API
-export([main/0]).



%% API
-define(PAR, ()).

main() ->
  foo(),
  foo?PAR,
  foo?PAR,
  io:format("hello").

foo() ->
  ok.
