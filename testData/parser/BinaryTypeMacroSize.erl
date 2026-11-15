-module(binaryTypeMacroSize).
-export_type([txs_hash/0, fr_bin/0, fu_bin/0]).

-define(TXS_HASH_BYTES, 32).
-define(FR_SIZE, 256).

-type(txs_hash() :: <<_:(?TXS_HASH_BYTES*8)>>).
-type fr_bin() :: <<_:?FR_SIZE>>.
-type fu_bin() :: <<_:_*?FR_SIZE>>.
