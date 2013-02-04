<warning>set_master_nodes</warning>(Nodes) when is_list(Nodes) ->
    UseDir = system_info(use_dir),
    IsRunning = system_info(is_running),
    case IsRunning of
	yes ->
	    CsPat = {{'_', cstruct}, '_'},
	    Cstructs0 = ?ets_match_object(mnesia_gvar, CsPat),
	    Cstructs = [Cs || {_, Cs} <- Cstructs0], 
	    log_valid_master_nodes(Cstructs, Nodes, UseDir, IsRunning);
	_NotRunning ->
	    case UseDir of
		true ->
		    mnesia_lib:<error>lock_table</error>(schema),
		    Res = 
			case mnesia_schema:<error>read_cstructs_from_disc</error>() of
			    {ok, Cstructs} ->
				log_valid_master_nodes(Cstructs, Nodes, UseDir, IsRunning);
			    {error, Reason} ->
				{error, Reason}
			end,
			mnesia_lib:<error>unlock_table</error>(schema),
		    Res;
		false ->
		    ok
	    end
    end;
set_master_nodes(Nodes) ->
    {error, {bad_type, Nodes}}.

log_valid_master_nodes(Cstructs, Nodes, UseDir, IsRunning) ->
    Fun = fun(Cs) ->
		  Copies = mnesia_lib:<error>copy_holders</error>(Cs),
		  Valid = mnesia_lib:<error>intersect</error>(Nodes, Copies),
		  {Cs#<error>cstruct</error>.name, Valid}
	  end,
    Args = lists:map(Fun, Cstructs),
    mnesia_recover:<error>log_master_nodes</error>(Args, UseDir, IsRunning).

<warning>set_master_nodes</warning>(Tab, Nodes) when is_list(Nodes) ->
    UseDir = system_info(use_dir),
    IsRunning = system_info(is_running),
    case IsRunning of
	yes ->
	    case ?catch_val({Tab, cstruct}) of
		{'EXIT', _} ->
		    {error, {no_exists, Tab}};
		Cs ->
		    case Nodes -- mnesia_lib:<error>copy_holders</error>(Cs) of
			[] ->
			    Args = [{Tab , Nodes}],
			    mnesia_recover:<error>log_master_nodes</error>(Args, UseDir, IsRunning);
			BadNodes ->
			    {error, {no_exists, Tab,  BadNodes}}
		    end
	    end;
	_NotRunning ->
	    case UseDir of
		true ->
		    mnesia_lib:<error>lock_table</error>(schema),
		    Res =
			case mnesia_schema:<error>read_cstructs_from_disc</error>() of
			    {ok, Cstructs} ->
				case lists:keysearch(Tab, 2, Cstructs) of
				    {value, Cs} ->
					case Nodes -- mnesia_lib:<error>copy_holders</error>(Cs) of
					    [] ->
						Args = [{Tab , Nodes}],
						mnesia_recover:<error>log_master_nodes</error>(Args, UseDir, IsRunning);
					    BadNodes ->
						{error, {no_exists, Tab,  BadNodes}}
					end;
				    false ->
					{error, {no_exists, Tab}}
				end;
			    {error, Reason} ->
				{error, Reason}
			end,
		    mnesia_lib:<error>unlock_table</error>(schema),
		    Res;
		false ->
		    ok
	    end
    end.