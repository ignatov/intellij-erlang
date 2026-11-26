-module('BinaryComprehensionTemplate').
-export([var/1, case_template/1, call_in_parens/1, macro_call/1, unbound/1]).

-define(BIN(X), <<X>>).

var(B) -> << B || _ <- [1] >>.

case_template(L) -> << case X of 1 -> <<1>>; _ -> <<0>> end || X <- L >>.

call_in_parens(L) -> << (to_bin(X)) || X <- L >>.

macro_call(L) -> << ?BIN(X) || X <- L >>.

unbound(L) -> << <error descr="Variable 'Y' is unbound">Y</error> || _ <- L >>.

to_bin(X) -> <<X>>.
