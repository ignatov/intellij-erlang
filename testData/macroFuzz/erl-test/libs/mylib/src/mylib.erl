%% Модуль библиотеки mylib. Включает свой заголовок относительным путём.
-module(mylib).

-export([version/0, opts/0, depth/1]).

-include("../include/mylib.hrl").

version() ->
  ?MYLIB_VERSION.

opts() ->
  #mylib_opts{depth = ?MYLIB_DEFAULT(depth)}.

depth(#mylib_opts{depth = D}) ->
  D.
