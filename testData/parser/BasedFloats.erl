-module(based_floats).

%% Plain floats keep working
plain_floats() ->
    3.14,
    1.0e10,
    2.5E-3.

%% Based floats in base 2 (EEP 75)
binary_based() ->
    2#0.111,
    2#0.10101#e8,
    2#0.1111_1111#E8.

%% Based floats in base 16
hex_based() ->
    16#ff.8,
    16#fefe.fefe#e16,
    16#0.011#e5.

%% Other bases, digits above f
other_bases() ->
    3#0.011,
    32#vrv.vrv#e15,
    36#z.z#e2.

%% Signed exponents
signed_exponents() ->
    16#1.8#e-1,
    2#1.0#e+3.

%% Based floats and radix integers mixed in expressions
in_expressions() ->
    X = 16#ff + 2#0.01,
    {16#dead, 16#d.e#e2, X}.
