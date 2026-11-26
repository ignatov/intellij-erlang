%% Заголовок для m_counter_server и m_counter_server_tests.
%% Запись состояния, константы и макросы для вызовов сервера.
-ifndef(COUNTER_HRL).
-define(COUNTER_HRL, true).

-define(SERVER, m_counter_server).
-define(DEFAULT_STEP, 1).
-define(MAX_VALUE, 1000).

-record(counter, {name :: atom(),
                  value = 0 :: non_neg_integer(),
                  step = ?DEFAULT_STEP :: pos_integer(),
                  history = [] :: [non_neg_integer()]}).

%% Вызов сервера: имя сервера подставляет макрос.
-define(CALL(Msg), gen_server:call(?SERVER, Msg)).
-define(CAST(Msg), gen_server:cast(?SERVER, Msg)).

-endif.
