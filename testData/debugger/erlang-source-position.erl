-module(erlang-source-position).

function() ->
  ok.

function_with_fun_expression() ->
  fun () ->
    ok
  end,
  ok.