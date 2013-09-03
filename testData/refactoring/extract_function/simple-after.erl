foo() ->
    {A, B, C} = {1, 2, 3},
    {D, E, F} = n(A, B, C),
    bar(D, E, F).

n(A, B, C) ->
{D, E, F} = foo(A, B, C),
bar(A),
1,
100,
{D, E, F}.