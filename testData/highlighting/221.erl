-export([foo/0, b/0, b2/0]).

foo() ->
    M = ?MODULE,
    Fun = fun M:bar/10,
    Fun.
b() ->
    Mod = ?MODULE,
    fun Mod:funname/0.

funname() ->
    ok.

b2() ->
    fun ?MODULE:funname/0.