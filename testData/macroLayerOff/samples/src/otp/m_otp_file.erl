%% Заголовок из OTP: kernel/include/file.hrl через -include_lib.
%% В нём запись file_info. Плагин находит приложение kernel в SDK
%% по файлу ebin/kernel.app.
%%
%% Что попробовать:
%% 1. Поставь курсор на `file_info` в `#file_info{}` и нажми Cmd+B.
%%    Должен открыться file.hrl из SDK.
%% 2. Поставь курсор на поле `size` и нажми Cmd+B.
%% 3. Набери `#file_info{` в `info/1` и нажми Ctrl+Space: должны быть поля.
%% 4. Поставь курсор на строку в -include_lib и нажми Cmd+B.
-module(m_otp_file).

-export([info/1, is_dir/1]).

-include_lib("kernel/include/file.hrl").

info(Path) ->
  case file:read_file_info(Path) of
    {ok, #file_info{size = Size, type = Type}} -> {Size, Type};
    {error, _} = E -> E
  end.

is_dir(#file_info{type = directory}) -> true;
is_dir(#file_info{}) -> false.
