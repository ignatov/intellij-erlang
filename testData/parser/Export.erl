set_master_nodes(Nodes) when is_list(Nodes) ->
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
		    mnesia_lib:lock_table(schema),
		    Res = 
			case mnesia_schema:read_cstructs_from_disc() of
			    {ok, Cstructs} ->
				log_valid_master_nodes(Cstructs, Nodes, UseDir, IsRunning);
			    {error, Reason} ->
				{error, Reason}
			end,
			mnesia_lib:unlock_table(schema),
		    Res;
		false ->
		    ok
	    end
    end;
set_master_nodes(Nodes) ->
    {error, {bad_type, Nodes}}.

log_valid_master_nodes(Cstructs, Nodes, UseDir, IsRunning) ->
    Fun = fun(Cs) ->
		  Copies = mnesia_lib:copy_holders(Cs),
		  Valid = mnesia_lib:intersect(Nodes, Copies),
		  {Cs#cstruct.name, Valid}  
	  end,
    Args = lists:map(Fun, Cstructs),
    mnesia_recover:log_master_nodes(Args, UseDir, IsRunning).

set_master_nodes(Tab, Nodes) when is_list(Nodes) ->
    UseDir = system_info(use_dir),
    IsRunning = system_info(is_running),
    case IsRunning of
	yes ->
	    case ?catch_val({Tab, cstruct}) of
		{'EXIT', _} ->
		    {error, {no_exists, Tab}};
		Cs ->
		    case Nodes -- mnesia_lib:copy_holders(Cs) of
			[] ->
			    Args = [{Tab , Nodes}],
			    mnesia_recover:log_master_nodes(Args, UseDir, IsRunning);
			BadNodes ->
			    {error, {no_exists, Tab,  BadNodes}}
		    end
	    end;
	_NotRunning ->
	    case UseDir of
		true ->
		    mnesia_lib:lock_table(schema),
		    Res = 
			case mnesia_schema:read_cstructs_from_disc() of
			    {ok, Cstructs} ->
				case lists:keysearch(Tab, 2, Cstructs) of
				    {value, Cs} ->
					case Nodes -- mnesia_lib:copy_holders(Cs) of
					    [] ->
						Args = [{Tab , Nodes}],
						mnesia_recover:log_master_nodes(Args, UseDir, IsRunning);
					    BadNodes ->
						{error, {no_exists, Tab,  BadNodes}}
					end;
				    false ->
					{error, {no_exists, Tab}}
				end;
			    {error, Reason} ->
				{error, Reason}
			end,
		    mnesia_lib:unlock_table(schema),
		    Res;
		false ->
		    ok
	    end
    end;
set_master_nodes(Tab, Nodes) ->
    {error, {bad_type, Tab, Nodes}}.
