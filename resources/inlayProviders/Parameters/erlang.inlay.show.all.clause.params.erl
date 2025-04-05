-module(calculator).

%% Function with multiple pattern-matching clauses
calculate(0, _Y) -> 0;
calculate(X, Y) when is_integer(X), is_integer(Y) -> 
    X + Y;
calculate(X, Y) when is_list(X), is_list(Y) ->
    string:concat(X, Y);
calculate(X, Y) ->
    {error, {unknown_types, X, Y}}.

%% Function that calls calculate
demo() ->
    IntResult = calculate(5, 10),
    StrResult = calculate("Hello, ", "Erlang!"),
    ZeroResult = calculate(0, 100),
    ErrorResult = calculate(1.5, true),
    {IntResult, StrResult, ZeroResult, ErrorResult}.