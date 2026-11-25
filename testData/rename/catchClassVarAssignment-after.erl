f() ->
    try error(oops)
    catch error:Ne<caret>w = oops -> New
    end.
