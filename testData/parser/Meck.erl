-module(meck).

shortcut_opaque_(Mod) ->
    ok = meck:expect(Mod, test, 0, {test, [a, self()]}),
    ?assertMatch({test, [a, P]} when P == self(), Mod:test()).