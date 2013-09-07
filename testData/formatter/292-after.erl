-export([ foo/0
        , bar_test/0, simple/1]).

-export_type([ card/0
             , out/0]).



foo() ->
  fun foo/0,

  A = 1,
  C = 10,

  S = case 1 of
        1 ->
          Str = "Hello",
          T = {a, b, c},
          L = [1, 2, T, Str],
          bar(Str),
          10
      end,

  io:format(ac(A + C + S)),

  {E, F, D} = {1, A, C},
  E, D, F,
  {A, C}.

-spec convert
    (tuple()) -> list();
    (list()) -> tuple().
convert(Tup) when is_tuple(Tup) -> tuple_to_list(Tup);
convert(L = [_|_]) -> list_to_tuple(L).

-spec kind(card()) -> face | number.
kind({_, A}) when A >= 1, A =< 10 -> number;
kind(_) -> face.

fo() -> [1231231, 3, 4, 14, 5].

foo(0) ->
  Var = {1, 1, 2},
  asd = 1,
  asd = 1,
  Bar = [X || X <- [1, 2, a, 3, 4,
                    b, 5, 6], is_integer(X), X > 3],
  Bar2 = [ 1, 2, 3
         , 4, 5, 6].


foo1() ->
  [{1, 2} = Rec, {1, 2} = Rec] = [{ 1
                                  , 2}, { 1
                                        , 2}].


-spec handle_result( flow_result()
                   , [service_spec()]) ->
                     flow_result() | { ok
                                     , cowboy_req:req()}.

handle_result({ flow, FlowName
              , Context}, _) ->
  ok;

handle_result( {flow, FlowName, Context}
             , _) -> ok.

foo() ->
  Context = 1,
  { ok
  , Flows} = unrest_context:get( flows
                               , Context).