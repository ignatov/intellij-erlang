f() ->
    try error(oops)
    catch error:New<caret> = oops -> New
    end.
