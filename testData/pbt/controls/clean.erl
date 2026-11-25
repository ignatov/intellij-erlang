-module(pbt).
-export([f1/2, f2/0]).
-export_type([t1/0]).
-record(r1, {a, b = 0 :: integer()}).
-type t1() :: integer() | atom().
-spec f1(t1(), [t1()]) -> ok.
f1(X, L) when is_integer(X) ->
    R = #r1{a = X},
    Y = R#r1.b,
    case [Z * 2 || Z <- L, Z > Y] of
        [] -> ok;
        [H | _] -> f3(H)
    end;
f1(_, _) ->
    ok.
f2() ->
    F = fun(A) -> A + 1 end,
    M = #{a => F(1)},
    #{a := V} = M,
    try V of
        1 -> ok
    catch
        error:_ -> ok
    end.
f3(B) ->
    receive
        {msg, B} -> ok
    after 0 -> <<B:8>>
    end.
