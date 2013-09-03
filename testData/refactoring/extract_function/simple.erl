foo() ->
    {A, B, C} = {1, 2, 3},
    <selection>{D, E, F} = foo(A, B, C),
    bar(A), 1, 100,</selection>
    bar(D, E, F).