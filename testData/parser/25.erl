-spec expect(Mod, Func, Expect) -> ok when
    Mod :: atom() | [atom()],              % <-- Mod is highlighted as an error
    Func :: atom(),
    Expect :: StubFun | [ClauseSpec],
    StubFun :: fun(),
    ClauseSpec :: {ArgPatterns, RetSpec},
    ArgPatterns :: [term() | '_'],
    RetSpec :: term() | ret_spec().