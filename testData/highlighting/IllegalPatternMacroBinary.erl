-export([f/1, g/1, bad/1, bar/1]).
-define(BYTE(X), X:8/unsigned-big-integer).
-define(UINT32(X), X:32/unsigned-big-integer).
-define(MSG, 34).

f(<<?BYTE(B), _/binary>>) -> B.

g(<<"dh_gex", ?BYTE(?MSG), ?UINT32(N)>>) -> N;
g(<<?BYTE(B), ?BYTE(254), ?BYTE(253), _/binary>>) -> B.

bar(_) -> ok.

bad(<<<error>bar(1)</error>, _/binary>>) -> ok;
bad(<<<error>?MODULE:bar(1)</error>, _/binary>>) -> ok.
