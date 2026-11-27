-export([g/0]).

g() ->
  B = <error descr="Variable 'B' is unbound">B</error>,
  B.
