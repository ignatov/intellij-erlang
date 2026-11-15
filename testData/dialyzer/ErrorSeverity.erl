-module('ErrorSeverity').
-export([f/0]).

f() -> g(1).
<error descr="Dialyzer: Function g/1 has no local return">g(a) -> ok.</error>
