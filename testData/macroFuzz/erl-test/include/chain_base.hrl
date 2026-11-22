%% Цепочка заголовков, звено 3 из 3. Тут нет других -include.
%%
%% Что попробовать:
%% 1. Поставь курсор на `BASE` и нажми Alt+F7. Должны найтись вхождения
%%    в chain_mid.hrl и в m_chain.erl, хотя m_chain.erl включает только
%%    chain_top.hrl.
-define(BASE, base).
-define(BASE_ARGS, (1, 2)).
-record(base_rec, {level = ?BASE}).
