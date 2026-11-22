-export([use/0, id/1, app_name/0]).

-define(REC(Name), -record(Name, {a = 1, b = 2})).
-define(EXPORT(Name), -export([Name/0])).
-define(ID_TYPE, non_neg_integer()).
-define(NAME_FUN, app_name() -> ok).
-define(UNUSED_FUN, helper() -> ok).

?REC(config).
?EXPORT(other).

-type id() :: ?ID_TYPE.

-spec id(id()) -> id().
id(X) -> X.

?NAME_FUN.

other() -> ok.

use() -> #config{a = 10}.
