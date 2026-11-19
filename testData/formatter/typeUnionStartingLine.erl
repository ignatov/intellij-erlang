-spec f(
    a
    | b,
    c) -> ok.

-spec g(
    a |
    b,
    c) -> ok.

-spec h(a()) ->
  ok
  | error.

-record(r, {f :: a
| b,
g = 1 :: c
| d}).

-spec i(X, Y) -> ok when X :: a
| b,
Y :: c
| d.
