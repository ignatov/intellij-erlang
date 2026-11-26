-module('UnboundUseBeforeMatch').
-export([f/0]).
f() -> <error descr="Variable 'X' is unbound">X</error>, X = ok.
