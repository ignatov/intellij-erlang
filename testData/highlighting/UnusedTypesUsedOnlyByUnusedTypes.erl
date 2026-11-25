-export([main/0]).

-type <warning descr="Unused type 't1'">t1</warning>() :: t1() | ok.
-type <warning descr="Unused type 't2'">t2</warning>() :: [t3()].
-type <warning descr="Unused type 't3'">t3</warning>() :: atom().
main() -> ok.
