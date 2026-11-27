-export([f/0, g/1, h/1]).
-record(foo, {a}).

%% a macro parameter after # is the record name only after the expansion
-define(NEW(Rec), #Rec{}).
-define(GET(Rec, R), R#Rec.a).
-define(UPD(Rec, R), R#Rec{a = 1}).

f() -> ?NEW(foo).
g(R) -> ?GET(foo, R).
h(R) -> ?UPD(foo, R).
