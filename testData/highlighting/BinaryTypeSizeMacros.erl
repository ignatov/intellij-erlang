-define(BYTES, 32).
-define(BITS(N), (N * 8)).

-export_type([hash/0, words/0, mixed/0, bad/0]).

-type hash() :: <<_:(?BYTES*8)>>.
-type words() :: <<_:_*?BITS(4)>>.
-type mixed() :: <<_:?BYTES, _:_*?BITS(1)>>.
-type bad() :: <<_:<error>?UNKNOWN</error> * 8>>.
