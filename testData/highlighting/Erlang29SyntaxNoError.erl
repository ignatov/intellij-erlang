-export([mirrored/0, tagged/0]).

%% OTP 29 syntax should not report Erlang29SyntaxInspection errors on OTP 29+

mirrored() ->
    [I, -I || I <- [1, 2, 3, 4, 5]].

tagged() ->
    [{left, I}, {right, I} || I <- [1, 2, 3, 4, 5]].
