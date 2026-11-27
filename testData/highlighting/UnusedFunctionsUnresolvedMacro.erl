-export([main/0]).
-include("<error descr="Unresolved include: file not found">missing.hrl</error>").
main() -> ?CALL.
<warning descr="Unused function 'g/0'">g</warning>() -> h().
h() -> ok.
