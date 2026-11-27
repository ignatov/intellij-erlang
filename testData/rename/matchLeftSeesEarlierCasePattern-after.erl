g(F) -> Zz = make_ref(), {ok, Zz} = case F(Zz) of {ok, Zz} -> {ok, Zz}; E -> E end, Zz.
