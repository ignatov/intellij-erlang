-export([foo/1, bar/0, assignment/0, case_test/1, receive_test/0, try_catch/1, list_comprehension/0,
                send/1, plusplus/1, comprehension/0, begin_end/0, random_test/1, generic_foo/0, rec/2]).

bar() -> ok.
foo(<error>A + 2</error>) -> A;
foo(<error>A * 2</error>) -> A;
foo(<error>A div 2</error>) -> A;
foo(<error>A orelse 2</error>) -> A;
foo(<error>A andalso 2</error>) -> A;
foo(<error>bar()</error>) -> ok;
foo(<error>{A, B} -- {1, 2}</error>) -> A;
foo(<error>bar() + 1</error>) -> ok;
foo(A = <error>B + 1</error>) -> {A, B};
foo(?MODULE_STRING) -> ok;
foo(?MODULE_STRING + 1) -> ok;
foo(<error>?MODULE_STRING + A</error>) -> A;
foo(<error>case A of {C} -> C end</error>) -> ok;
foo(<error>catch 1</error>) -> ok;
foo(<error>fun (X) -> ok end</error>) -> ok;
foo(<error>(fun(A) -> A = 2 end)(2)</error>) -> ok;
foo(<error>if ok -> ok end</error>)  -> ok;
foo(<error>-X</error>) -> ok;
foo(-(5 + 3)) -> ok.

assignment() ->
  <error>A + 1</error> = 5, A;
assignment() ->
  A = 5, A.

case_test(A) ->
  case A of {<error>C + 1</error>} -> C
  end;
case_test(A) ->
  case A of {D} -> D + 1
  end.

receive_test() ->
  receive <error>B + 1</error> -> B
  end;
receive_test() ->
  receive B -> B * 2
  end.

try_catch(A) ->
  try A catch <error>A + 1</error> -> A;
  exit:Reason -> {'EXIT', Reason + 1}
  end.

list_comprehension() ->
  [X*2 || X <- [1,3]],
  [X*2 || <error>X + 1</error> <- [1,3]],
  X = 10, <error>[A || A <- [94]]</error> = X.

send(<error>A = C ! B</error>) -> A;
send(A) -> B = 15, A ! B;
send(A) -> B = 15, C = A ! B, C;
send(A) -> C = 15, A ! B = C, B.

plusplus(A) ->
  "abc" ++ R = A, R;
plusplus(A) ->
  <error>"abc" + " " ++ R</error> = A, R;
plusplus(A) ->
  <error>R ++ "abc"</error> = A, R;
plusplus(A) ->
  "qwe" ++ "rty" ++ A = 12, A;
plusplus(A) ->
  <error>"qwe" ++ "qwe" + 1 ++ A</error> = 12, A;
plusplus(A) ->
  "qwe" ++ 1 = 12, A.

comprehension() ->
  [A || <error>A + 1</error> <- []];
comprehension() ->
  [A || A <- []].

begin_end() ->
  <error>begin A = 10 end</error> = 5,
  A = begin C = 10 end, C.

generic_foo() ->
  X = fun(A) -> A end,
  {<error>X(A)</error>} = {1, 2}, X.

-record(data, {}).
rec(Record, #data{}=A) ->
  <error>Record#data{}</error> = {}, A.

random_test(A) ->
  (A + 1):abs(1 + A);
random_test([1, B]) ->
  B = [1 + B], [<error>1 + B</error>] = B.
