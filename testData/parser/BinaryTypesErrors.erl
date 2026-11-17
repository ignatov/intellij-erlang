-module('BinaryTypesErrors').

-type t1() :: <<_:_*8, _:8>>.
-type t2() :: <<_:8, _:8>>.
-type t3() :: <<_:8, _:_*8, _:8>>.
-type t4() :: <<_:8,>>.
-type t5() :: <<_:8.0>>.
