-module(binary_type_size_expressions).

-define(TXS_HASH_BYTES, 32).
-define(FR_SIZE, 256).
-define(BYTES(N), (N * 8)).

-export_type([txs_hash/0, fr_bin/0, fr_bins/0, both/0, t/0, s/0, u/0, w/0, c/0, n/0]).

-type(txs_hash() :: <<_:(?TXS_HASH_BYTES*8)>>).
-type fr_bin() :: <<_:?FR_SIZE>>.
-type fr_bins() :: <<_:_*?FR_SIZE>>.
-type both() :: <<_:?BYTES(2), _:_*?BYTES(1)>>.
-type t() :: <<_:(4*8)>>.
-type s() :: <<_:4*8>>.
-type u() :: <<_:_*(1 bsl 3)>>.
-type w() :: <<_:(?FR_SIZE - 8) div 2 + 1 band 7>>.
-type c() :: <<_:$a>>.
-type n() :: <<_:8>> | <<_:_*8>> | <<>>.
