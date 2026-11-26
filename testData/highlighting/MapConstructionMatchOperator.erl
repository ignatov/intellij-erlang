-module('MapConstructionMatchOperator').
-export([f/0, g/1, h/0, i/1, ok/1, ok2/1]).

f() -> #{a <error>:=</error> 1}.
g(M) -> M#{a => #{b <error>:=</error> 1}}.
h() -> [#{a => 1, b <error>:=</error> 2}].
i(M) -> case M of #{a := X} -> #{x <error>:=</error> X} end.

ok(M) -> M#{a := 1}.
ok2(#{a := X}) -> X.
