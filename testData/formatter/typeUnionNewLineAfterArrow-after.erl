-spec api_error(rd()) ->
  ok
  | {error, error_reason()}
  | {error, [error_reason()]}.

-spec f() ->
  a |
  b.

-callback g(a()) ->
  ok
  | error.

-spec h(a) ->
  ok
  | error;
    (b) ->
  x
  | y.

-type t() :: fun((a) ->
  b
  | c).
