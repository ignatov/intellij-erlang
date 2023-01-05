-spec foo(A :: any(), B :: binary(), C :: tuple()) -> atom().
foo(A, <<"binbin">> = B, C = {tuple}) -> ok.