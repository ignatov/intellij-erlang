-spec f([a
| b], c) -> ok.

-spec g(x(), [a
| b]) -> ok.

-spec h(#{k => a
| b}) -> ok.

-record(r, {f :: a
| b,
g = 1 :: c
| d}).

-type t() :: {x, [a
| b]}.
