-module('UnboundUseBeforeMatchScopes').
-export([twice/0, in_case/1, in_begin/0, bind_in_fun/0, bind_in_case/1, bind_in_filter/0, case_pattern/1,
  use_in_fun/0, use_in_comprehension/0, other_branch/1, other_if_branch/1, bind_in_fun_before/0, fun_argument/0,
  other_fun_clause/0, same_fun_clause/0, generator_expression/0, binary_generator_expression/1,
  same_generator_expression/0]).
twice() -> <error descr="Variable 'X' is unbound">X</error>, <error descr="Variable 'X' is unbound">X</error>, X = ok.
in_case(A) -> case A of 1 -> <error descr="Variable 'Y' is unbound">Y</error>; _ -> ok end, Y = ok.
in_begin() -> begin <error descr="Variable 'V' is unbound">V</error> end, V = ok.
bind_in_fun() -> <error descr="Variable 'X' is unbound">X</error>, fun() -> X = 1 end.
bind_in_case(A) -> <error descr="Variable 'Y' is unbound">Y</error>, case A of 1 -> Y = 1; _ -> ok end.
bind_in_filter() -> <error descr="Variable 'Z' is unbound">Z</error>, [ok || _ <- [1], Z = 1].
case_pattern(A) -> <error descr="Variable 'X' is unbound">X</error>, case A of X -> ok end.
use_in_fun() -> fun() -> <error descr="Variable 'W' is unbound">W</error> end, <warning descr="Unused variable 'W'">W</warning> = ok.
use_in_comprehension() -> [<error descr="Variable 'U' is unbound">U</error> || _ <- [1]], <warning descr="Unused variable 'U'">U</warning> = ok.
other_branch(A) -> case A of 1 -> <error descr="Variable 'Q' is unbound">Q</error>; _ -> <warning descr="Unused variable 'Q'">Q</warning> = 1 end.
other_if_branch(A) -> if A -> <error descr="Variable 'O' is unbound">O</error>; true -> <warning descr="Unused variable 'O'">O</warning> = 1 end.
bind_in_fun_before() -> fun() -> <warning descr="Unused variable 'X'">X</warning> = 1 end, <error descr="Variable 'X' is unbound">X</error>.
fun_argument() -> <error descr="Variable 'X' is unbound">X</error>, fun(<warning descr="Unused variable 'X'">X</warning>) -> ok end.
other_fun_clause() -> fun(1) -> <error descr="Variable 'X' is unbound">X</error>; (2) -> <warning descr="Unused variable 'X'">X</warning> = 1 end.
same_fun_clause() -> fun(1) -> <error descr="Variable 'X' is unbound">X</error>, X = 1; (2) -> ok end.
generator_expression() -> [ok || _ <- [<error descr="Variable 'X' is unbound">X</error>], <warning descr="Unused variable 'X'">X</warning> = 1].
binary_generator_expression(B) -> << <<1>> || <<_>> <= <error descr="Variable 'X' is unbound">X</error>, <warning descr="Unused variable 'X'">X</warning> = B >>.
same_generator_expression() -> [ok || _ <- [<error descr="Variable 'X' is unbound">X</error>, X = 1]].
