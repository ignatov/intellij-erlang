-export([reported/0, var_on_right/0, var_in_tuple/0]).

reported() ->
    try error(oops)
    catch error:Reason = oops -> Reason
    end.

var_on_right() ->
    try error(oops)
    catch error:oops = Reason -> Reason
    end.

var_in_tuple() ->
    try error({oops})
    catch error:{R} = _ -> R
    end.
