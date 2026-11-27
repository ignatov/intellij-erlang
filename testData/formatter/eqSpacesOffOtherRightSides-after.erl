-record(t, {a= <<<<C>> || <<C>> <= <<"ab">>>> :: binary(), b=1}).
f(Y) -> X=Y, B= <<1>>, R=#t{b=Y}, T=#t{a= <<1>> =:= Y}, {X, B, R, T}.
