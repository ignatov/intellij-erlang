-export([b1/1, b2/1, b3/1]).

b1(A) -> case A of _ -> case A of X -> ok end, X end.

b2(A) -> case A of _ -> case A of {ok, X} -> ok; {error, X} -> ok end, X end.

b3(A) -> case A of X -> ok end, X.
