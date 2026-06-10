-export([mirrored/0, tagged/0]).

%% OTP 29 syntax should report errors on SDK < 29

mirrored() ->
    [I, <error descr="Multi-valued comprehensions are only supported in Erlang 29 and newer versions">-I</error> || I <- [1, 2, 3, 4, 5]].

tagged() ->
    [{left, I}, <error descr="Multi-valued comprehensions are only supported in Erlang 29 and newer versions">{right, I}</error> || I <- [1, 2, 3, 4, 5]].
