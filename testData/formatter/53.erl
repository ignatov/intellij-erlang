-module(aaa).

%last line not shifted.
f() ->
    [X || Z <- [1, 2],
        X <- Z].

%too much shift
to_stream(L) when is_list(L) ->
    stream(L, fun
        ([]) -> empty;
        ([H | T]) -> {H, T}
    end).

%comment should be aligned as well
f() ->
    % comment line
    f().