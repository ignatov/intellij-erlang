f() ->
  <caret>MyFunc = 'my-func'(1),
  g(MyFunc).

g(X) -> X.

'my-func'(X) -> X.