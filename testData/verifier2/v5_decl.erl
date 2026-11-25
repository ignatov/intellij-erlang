-module(v5_decl).
-export([name_fun/0, use_rec/0, real/1]).
-define(REC(Name), -record(Name, {a = 1, b})).
-define(FUN(Name), Name() -> ok).
-define(SPEC(F), -spec F(integer()) -> integer()).

?REC(vrec).

?FUN(name_fun).

use_rec() -> #vrec{a = 2}.

?SPEC(real).
real(X) -> X + 1.
