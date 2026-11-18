-export([f/2, g/2, h/1, i/1]).
-record(r, {a}).

f(X, M) -> #{#r{a = X + 1} := V} = M, V.
g(X, M) ->
  case M of
    #{#r{a = -X} := V} -> V;
    #{#r{a = element(1, X)} := V} -> V;
    #{#r{a = hd(X)} := V} -> V;
    #{#r{a = length(X)} := V} -> V;
    #{#r{a = X =:= 1} := V} -> V;
    #{#r{a = X andalso true} := V} -> V;
    #{#r{a = X#r.a} := V} -> V;
    #{#r{a = {X + 1}} := V} -> V;
    #{{#r{a = X + 1}} := V} -> V;
    #{[#r{a = X + 1}] := V} -> V;
    #{#{a => #r{a = X + 1}} := V} -> V
  end.
h(#{#r{a = self()} := V}) -> V.
i(#{#r{a = #r{a = <error>"a" ++ "b"</error>}} := V}) -> V.
