-module(otp29_comprehensions).

mirrored() ->
    [I, -I || I <- lists:seq(1, 5)].

tagged() ->
    [{left, I}, {right, I} || I <- lists:seq(1, 5)].

nested() ->
    [[X, Y || Y <- Ys] || {X, Ys} <- [{a, [1, 2]}, {b, [3]}]].

assignment_filter() ->
    [H || E <- [1, 2, 3], H = E + 1, H > 2].
