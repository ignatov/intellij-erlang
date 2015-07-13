-export([zoo/1, abs/1, foo/1, doo/1, roo/1]).

zoo(A) -> A.
abs(A) -> A.

foo(A) when A orelse 3 > 5 -> ok;
foo(A) when A + 3 -> ok;
foo(A) when <error>A = 10</error> -> ok;
foo(A) when <error>case A of A -> A end</error> -> ok;
foo(A) when <error>try A catch A -> ok end</error> -> ok;
foo(A) when <error>begin A end</error> -> ok;
foo(A) when <error>A ! A</error> -> ok;
foo(A) when [1, 2 + A, 3] > A -> ok;
foo(A) when <error>3 ++ A</error> > 4 -> ok;
foo(A) when <error>[X*2 || X <- [1,3]]</error> -> A;
foo(A) when <error>receive A -> A end</error> -> ok;
foo(A) when <error>B = fun (X) -> X end</error>, B -> A;
foo(A) when <error>(fun(B) -> B end)(2)</error> -> A;
foo(A) when <error>catch 1</error> -> A;
foo(A) when <error>if ok -> ok end</error> -> A;
foo(A) when <error>X = fun(A) -> A end</error>, <error>X(A)</error> -> A;

foo(A) when is_boolean(A) -> ok;
foo(A) when <error>apply(A, A)</error> -> ok;
foo(A) when <error>zoo(A)</error> -> ok;
foo(A) when <error><error>abs</error>(A)</error> -> ok;
foo(A) when <error>my_module:abs(A)</error> -> ok;
foo(A) when erlang:abs(A) -> ok;
foo(A) when erlang:<error>apply(A, A)</error> -> ok;
foo(A) when is_boolean(<error>zoo(A)</error>) -> ok.

-define(Q, 15).
-define(do_something(A), A).
-define(do_nothing(A), ok).

doo(A) when ?do_nothing(A) + ?Q -> ok;
doo(A) when ?do_nothing(zoo(A)) -> ok.

roo(A) ->
  if is_boolean(12) -> ok;
    <error>apply(1, 2)</error> -> ok
  end, A;
roo(A) ->
  case A of A when <error>A = 10</error> -> A end;
roo(A) ->
  receive A when <error>3 ++ A</error> -> A end;
roo(A) ->
  try A of A when <error>A ! A</error> -> A
  catch A when <error>A = 10</error> -> ok end;
roo(A) ->
  B = fun (X) when <error>A = 10</error> -> X end, B.