-module(heads).
-export([a/0, b/0, c/0, d/0, e/0, f/1, g/0]).
-record(r, {x}).
-define(DEF(Name), Name() -> ok).

a() -> ok.
%% a comment
b() -> #r{x = 1}#r.x.
-ifdef(TEST).
c() -> test.
-else.
c() -> prod.
-endif.
-spec d() -> ok.
d() ->
  ok.
?DEF(x).
e() -> 16#ff.
f(_) -> ok.
g() -> ok;
g() -> other.
