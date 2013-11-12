-module(erlang).

-export([apply/2,apply/3,spawn/4,spawn_link/4,
	 spawn_monitor/1,spawn_monitor/3,
	 spawn_opt/2,spawn_opt/3,spawn_opt/4,spawn_opt/5]).
-export([spawn/1, spawn_link/1, spawn/2, spawn_link/2]).

-deprecated([hash/2]).

% Get rid of autoimports of spawn to avoid clashes with ourselves.
-compile({no_auto_import,[spawn/1]}).
-compile({no_auto_import,[spawn/4]}).
-compile({no_auto_import,[spawn_link/1]}).
-compile({no_auto_import,[spawn_link/4]}).
-compile({no_auto_import,[spawn_opt/2]}).
-compile({no_auto_import,[spawn_opt/4]}).
-compile({no_auto_import,[spawn_opt/5]}).

-export_type([timestamp/0]).

-type timestamp() :: {MegaSecs :: non_neg_integer(),
                      Secs :: non_neg_integer(),
                      MicroSecs :: non_neg_integer()}.

%%--------------------------------------------------------------------------

-spec apply(Fun, Args) -> term() when
      Fun :: function(),
      Args :: [term()].
apply(Fun, Args) ->
    erlang:apply(Fun, Args).

-spec apply(Module, Function, Args) -> term() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
apply(Mod, Name, Args) ->
    erlang:apply(Mod, Name, Args).

%% Spawns with a fun

-spec spawn(Fun) -> pid() when
      Fun :: function().
spawn(F) when is_function(F) ->
    spawn(erlang, apply, [F, []]);
spawn({M,F}=MF) when is_atom(M), is_atom(F) ->
    spawn(erlang, apply, [MF, []]);
spawn(F) ->
    erlang:error(badarg, [F]).

-spec spawn(Node, Fun) -> pid() when
      Node :: node(),
      Fun :: function().
spawn(N, F) when N =:= node() ->
    spawn(F);
spawn(N, F) when is_function(F) ->
    spawn(N, erlang, apply, [F, []]);
spawn(N, {M,F}=MF) when is_atom(M), is_atom(F) ->
    spawn(N, erlang, apply, [MF, []]);
spawn(N, F) ->
    erlang:error(badarg, [N, F]).

-spec spawn_link(Fun) -> pid() when
      Fun :: function().
spawn_link(F) when is_function(F) ->
    spawn_link(erlang, apply, [F, []]);
spawn_link({M,F}=MF) when is_atom(M), is_atom(F) ->
    spawn_link(erlang, apply, [MF, []]);
spawn_link(F) ->
    erlang:error(badarg, [F]).

-spec spawn_link(Node, Fun) -> pid() when
      Node :: node(),
      Fun :: function().
spawn_link(N, F) when N =:= node() ->
    spawn_link(F);
spawn_link(N, F) when is_function(F) ->
    spawn_link(N, erlang, apply, [F, []]);
spawn_link(N, {M,F}=MF) when is_atom(M), is_atom(F) ->
    spawn_link(N, erlang, apply, [MF, []]);
spawn_link(N, F) ->
    erlang:error(badarg, [N, F]).

%% Spawn and atomically set up a monitor.

-spec spawn_monitor(Fun) -> {pid(), reference()} when
      Fun :: function().
spawn_monitor(F) when is_function(F, 0) ->
    erlang:spawn_opt({erlang,apply,[F,[]],[monitor]});
spawn_monitor(F) ->
    erlang:error(badarg, [F]).

-spec spawn_monitor(Module, Function, Args) -> {pid(), reference()} when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
spawn_monitor(M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    erlang:spawn_opt({M,F,A,[monitor]});
spawn_monitor(M, F, A) ->
    erlang:error(badarg, [M,F,A]).

-spec spawn_opt(Fun, Options) -> pid() | {pid(), reference()} when
      Fun :: function(),
      Options :: [Option],
      Option :: link | monitor | {priority, Level}
              | {fullsweep_after, Number :: non_neg_integer()}
              | {min_heap_size, Size :: non_neg_integer()}
              | {min_bin_vheap_size, VSize :: non_neg_integer()},
      Level :: low | normal | high.
spawn_opt(F, O) when is_function(F) ->
    spawn_opt(erlang, apply, [F, []], O);
spawn_opt({M,F}=MF, O) when is_atom(M), is_atom(F) ->
    spawn_opt(erlang, apply, [MF, []], O);
spawn_opt({M,F,A}, O) -> % For (undocumented) backward compatibility
    spawn_opt(M, F, A, O);
spawn_opt(F, O) ->
    erlang:error(badarg, [F, O]).

-spec spawn_opt(Node, Fun, Options) -> pid() | {pid(), reference()} when
      Node :: node(),
      Fun :: function(),
      Options :: [Option],
      Option :: link | monitor | {priority, Level}
              | {fullsweep_after, Number :: non_neg_integer()}
              | {min_heap_size, Size :: non_neg_integer()}
              | {min_bin_vheap_size, VSize :: non_neg_integer()},
      Level :: low | normal | high.
spawn_opt(N, F, O) when N =:= node() ->
    spawn_opt(F, O);
spawn_opt(N, F, O) when is_function(F) ->
    spawn_opt(N, erlang, apply, [F, []], O);
spawn_opt(N, {M,F}=MF, O) when is_atom(M), is_atom(F) ->
    spawn_opt(N, erlang, apply, [MF, []], O);
spawn_opt(N, F, O) ->
    erlang:error(badarg, [N, F, O]).

%% Spawns with MFA

-spec spawn(Node, Module, Function, Args) -> pid() when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
spawn(N,M,F,A) when N =:= node(), is_atom(M), is_atom(F), is_list(A) ->
    spawn(M,F,A);
spawn(N,M,F,A) when is_atom(N), is_atom(M), is_atom(F) ->
    case is_well_formed_list(A) of
	true ->
	    ok;
	false ->
	    erlang:error(badarg, [N, M, F, A])
    end,
    case catch gen_server:call({net_kernel,N},
			       {spawn,M,F,A,group_leader()},
			       infinity) of
	Pid when is_pid(Pid) ->
	    Pid;
	Error ->
	    case remote_spawn_error(Error, {no_link, N, M, F, A, []}) of
		{fault, Fault} ->
		    erlang:error(Fault, [N, M, F, A]);
		Pid ->
		    Pid
	    end
    end;
spawn(N,M,F,A) ->
    erlang:error(badarg, [N, M, F, A]).

-spec spawn_link(Node, Module, Function, Args) -> pid() when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
spawn_link(N,M,F,A) when N =:= node(), is_atom(M), is_atom(F), is_list(A) ->
    spawn_link(M,F,A);
spawn_link(N,M,F,A) when is_atom(N), is_atom(M), is_atom(F) ->
    case is_well_formed_list(A) of
	true ->
	    ok;
	_ ->
	    erlang:error(badarg, [N, M, F, A])
    end,
    case catch gen_server:call({net_kernel,N},
			       {spawn_link,M,F,A,group_leader()},
			       infinity) of
	Pid when is_pid(Pid) ->
	    Pid;
	Error ->
	    case remote_spawn_error(Error, {link, N, M, F, A, []}) of
		{fault, Fault} ->
		    erlang:error(Fault, [N, M, F, A]);
		Pid ->
		    Pid
	    end
    end;
spawn_link(N,M,F,A) ->
    erlang:error(badarg, [N, M, F, A]).

-spec spawn_opt(Module, Function, Args, Options) ->
                       pid() | {pid(), reference()} when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Options :: [Option],
      Option :: link | monitor | {priority, Level}
              | {fullsweep_after, Number :: non_neg_integer()}
              | {min_heap_size, Size :: non_neg_integer()}
              | {min_bin_vheap_size, VSize :: non_neg_integer()},
      Level :: low | normal | high.
spawn_opt(M, F, A, Opts) ->
    case catch erlang:spawn_opt({M,F,A,Opts}) of
	{'EXIT',{Reason,_}} ->
	    erlang:error(Reason, [M,F,A,Opts]);
	Res ->
	    Res
    end.

-spec spawn_opt(Node, Module, Function, Args, Options) ->
                       pid() | {pid(), reference()} when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Options :: [Option],
      Option :: link | monitor | {priority, Level}
              | {fullsweep_after, Number :: non_neg_integer()}
              | {min_heap_size, Size :: non_neg_integer()}
              | {min_bin_vheap_size, VSize :: non_neg_integer()},
      Level :: low | normal | high.
spawn_opt(N, M, F, A, O) when N =:= node(),
			      is_atom(M), is_atom(F), is_list(A),
			      is_list(O) ->
    spawn_opt(M, F, A, O);
spawn_opt(N, M, F, A, O) when is_atom(N), is_atom(M), is_atom(F) ->
    case {is_well_formed_list(A), is_well_formed_list(O)} of
	{true, true} ->
	    ok;
	_ ->
	    erlang:error(badarg, [N, M, F, A, O])
    end,
    case lists:member(monitor, O) of
	false -> ok;
	true -> erlang:error(badarg, [N, M, F, A, O])
    end,
    {L,NO} = lists:foldl(fun (link, {_, NewOpts}) ->
				 {link, NewOpts};
			     (Opt, {LO, NewOpts}) ->
				 {LO, [Opt|NewOpts]}
			 end,
			 {no_link,[]},
			 O),
    case catch gen_server:call({net_kernel,N},
			       {spawn_opt,M,F,A,NO,L,group_leader()},
			       infinity) of
	Pid when is_pid(Pid) ->
	    Pid;
	Error ->
	    case remote_spawn_error(Error, {L, N, M, F, A, NO}) of
		{fault, Fault} ->
		    erlang:error(Fault, [N, M, F, A, O]);
		Pid ->
		    Pid
	    end
    end;
spawn_opt(N,M,F,A,O) ->
    erlang:error(badarg, [N,M,F,A,O]).

is_record(_Term,_RecordTag) ->
    erlang:nif_error(undefined).

is_record(_Term,_RecordTag,_Size) ->
    erlang:nif_error(undefined).