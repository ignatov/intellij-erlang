-export([f/1, bar/0, bar/2, buzz/0, rec/0, mrec/0]).

f(A) -> A.
foo() -> f(10).
buzz() -> foo().

-spec <warning>foo</warning>(0,0,0,0) -> ok.
-spec test:<warning>bar</warning>/0 :: () -> ok.
-spec test:<warning>bar</warning><error> </error>:: () -> ok.
-spec bar/0 :: () -> ok.
-spec bar/2 :: (1,1) -> ok ;
               (1,2) -> ok.

bar() ->
  fun <warning>foo</warning>/4.

bar(1,1) -> ok.

-define(M(X), X).
rec() -> <warning>rec</warning>(<warning>rec</warning>(1)).
mrec() -> ?M(<warning>rec</warning>(1)).