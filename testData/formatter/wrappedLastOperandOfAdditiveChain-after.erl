call(A, B, C, D) ->
  Y = A +
    B + foo(C,
      D),
  Y.

parenthesized(A, C, D, E) ->
  X = A +
    C + (D
      - E),
  X.

list_and_tuple(A, B, C, D) ->
  W = A +
    B + [C,
      D] + {C,
      D},
  W.

multiplication(A, B, C, D) ->
  U = A +
    B + C *
      D,
  U.

map(A, B, C, D) ->
  S = A +
    B + #{a => C,
      b => D},
  S.

middle_operand(A, B, C, D) ->
  R = A - B
    - foo(C,
      D) - 1,
  R.

case_and_fun(A, B, C) ->
  V = A +
    B + case C of
          1 -> 2;
          _ -> 3
        end,
  F = A +
    B + fun() ->
          C
        end,
  {V, F}.
