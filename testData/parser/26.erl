-record(nrec0, {name = "nested0"}).
-record(nrec1, {name = "nested1", nrec0=#nrec0{}}).
-record(nrec2, {name = "nested2", nrec1=#nrec1{}}).

f() ->
    N2 = #nrec2{},
    "nested0" = N2#nrec2.nrec1#nrec1.nrec0#nrec0.name,
    N0n = N2#nrec2.nrec1#nrec1.nrec0#nrec0{name = "nested0a"}.