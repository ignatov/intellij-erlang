-module(w2_fun).
-export([use/0, hello/0, world/0]).

-define(GETTER(N), N() -> N).

?GETTER(hello).
?GETTER(world).

use() -> {hello(), world(), hello()}.
