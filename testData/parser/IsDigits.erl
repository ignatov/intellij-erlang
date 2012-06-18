is_digits([Dig | Tail]) ->
    if
	$0 =< Dig, Dig =< $9 ->
	    is_digits(Tail);
	true ->
	    false
    end;
is_digits([]) ->
    true.