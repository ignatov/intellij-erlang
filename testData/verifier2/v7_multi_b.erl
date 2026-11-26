-module(v7_multi_b).
-export([u/0]).

u() -> {v7_multi_a:shared(), v7_multi_a:plain()}.
