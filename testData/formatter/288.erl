f() ->
                    %% asfa
    try
        io:format("abc")
    catch
                     %% aff
        Ex -> io:format("error ~p", [Ex])
    after
                    %% some comment
        io:format("after"),
        foo()
                     %% some comment
    end.