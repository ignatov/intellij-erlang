-module('BinaryTypes').

-export_type([t1/0, t2/0, t3/0, t4/0, t5/0]).

-type t1() :: <<>>.
-type t2() :: <<_:8>>.
-type t3() :: <<_:_*8>>.
-type t4() :: <<_:8, _:_*8>>.
-type t5() :: <<_:0, _:_*1>> | binary().
