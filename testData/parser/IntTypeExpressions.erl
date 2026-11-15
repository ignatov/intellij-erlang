-module('IntTypeExpressions').

-define(P, 100).
-define(MWC59_P, ((16#7fa6502 bsl 32) - 1)).

-export_type([t1/0, t2/0, t3/0, t4/0, t5/0, t6/0, t7/0, t8/0, t9/0, t10/0, t11/0, t12/0, t13/0, t14/0]).

-type t1() :: 1..10-1.
-type t2() :: 0..1 bsl 8.
-type t3() :: 0..(1 bsl 8).
-type t4() :: 1 bsl 8.
-type t5() :: 1..?MWC59_P-1.
-type t6() :: 1..9 | -1..1 | 1..?P.
-type t7() :: (1 bsl 8)..300 | (1) + 2.
-type t8() :: - - 1 | +1 | bnot 1 | -(1) | 1 - -1.
-type t9() :: 1 div 2 rem 3 band 4 bor 5 bxor 6 bsr 1 * 2.
-type t10() :: $a..$z - 1.
-type t11() :: 1 + (2 * 3) | ((1)).
-type t12() :: X :: 1 + 2 | atom().
-type t13() :: [1 + 2] | {1 bsl 2, a} | (atom() | 1).
-type t14() :: fun((1..3 - 1) -> 0..1 bsl 8).
