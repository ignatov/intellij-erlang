-export([f/1]).

-record(r, {a, b}).

-define(CREATE_RD(KV_T_La, Record),
        begin
            RD = #Record{},
            PL = record_info(fields,Record),
            I_L = lists:zip(PL, lists:seq(2, length(PL)+1)),
            InsertL = [{binary_to_existing_atom(BK,latin1), V} || {BK, V} <- KV_T_La],
            update_tuple(InsertL, I_L, RD)
        end
).

f(L) -> ?CREATE_RD(L, r).

update_tuple(_InsertL, _I_L, RD) -> RD.
