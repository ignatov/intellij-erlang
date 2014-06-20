test1() ->
  Result = try
             io:format("getting random number\n"),
             random:uniform()
           catch
             throw:oops -> false;
             error:badarg -> false
           end,
  {ok, Result}.

test2() ->
  Result =
    try
      io:format("getting random number\n"),
      random:uniform()
    catch
      throw:oops -> false;
      error:badarg -> false
    end,
  {ok, Result}.