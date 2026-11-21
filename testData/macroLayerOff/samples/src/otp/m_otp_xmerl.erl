%% Заголовок из OTP: xmerl/include/xmerl.hrl.
%% В нём записи xmlElement и xmlText и макросы-обрывки для охраны:
%% whitespace(H) раскрывается в `H==?space ; H==?cr ; ...`, то есть
%% в несколько охран через `;`.
%%
%% Что попробовать:
%% 1. Поставь курсор на `whitespace` в охране `skip/1` и нажми Cmd+B.
%% 2. Поставь курсор на `xmlElement` и нажми Cmd+B.
%% 3. Поставь курсор на поле `content` и нажми Cmd+B.
%% 4. Набери `#xmlText{` и нажми Ctrl+Space.
-module(m_otp_xmerl).

-export([skip/1, texts/1]).

-include_lib("xmerl/include/xmerl.hrl").

skip([H | T]) when ?whitespace(H) -> skip(T);
skip(L) -> L.

texts(#xmlElement{content = Content}) ->
  [V || #xmlText{value = V} <- Content];
texts(_) ->
  [].
