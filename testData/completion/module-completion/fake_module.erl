-module(fake_module).

-export([bar/0, bar/1, tar/0]).

bar() -> ok.
bar(1) -> ok.
tar() -> ok
far() -> not_ok.