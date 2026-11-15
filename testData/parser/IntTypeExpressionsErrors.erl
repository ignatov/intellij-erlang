-module('IntTypeExpressionsErrors').

-type t1() :: 1..2.0.
-type t2() :: 1..a.
-type t3() :: 1..2..3.
-type t4() :: 1 + atom().
-type t5() :: 1 / 2.
-type t6() :: 1 band (1 | 2).
