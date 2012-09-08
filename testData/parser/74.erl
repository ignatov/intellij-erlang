-record(rec, {a, b, c}).

f() ->
    V = #rec{a=1, _='abc', c=abs()}.