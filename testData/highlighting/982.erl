-export([handle_call/3]).

handle_call(_Request, _From, State) ->
  case VarB = fun_b() of
    a -> a;
    _ -> VarB
  end,
  {reply, ok, State}.

fun_b() -> b.
