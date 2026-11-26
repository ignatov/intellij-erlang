f() ->
    try error(oops)
    catch error:Rea<caret>son = oops -> Reason
    end.
