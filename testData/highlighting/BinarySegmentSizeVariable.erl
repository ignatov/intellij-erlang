-export([fun_head/1, generator/1, body_match/1, case_pattern/1, function_head/2,
         bound_generator/2, bound_body_match/2, bound_case_pattern/2]).

fun_head(V) ->
  fun(<<B:V/integer>>) -> B end.

generator(L) ->
  [S || <<S:<error descr="Variable 'Y' is unbound">Y</error>>> <- L].

body_match(B) ->
  <<X:<error descr="Variable 'N' is unbound">N</error>>> = B, X.

case_pattern(B) ->
  case B of
    <<X:<error descr="Variable 'M' is unbound">M</error>>> -> X
  end.

function_head(N, <<X:<error descr="Variable 'N' is unbound">N</error>>>) -> {N, X}.

bound_generator(Y, L) ->
  [S || <<S:Y>> <- L].

bound_body_match(N, B) ->
  <<X:N>> = B, X.

bound_case_pattern(M, B) ->
  case B of
    <<X:M>> -> X
  end.
