-record(state,
    {callgraph            :: dialyzer_callgraph:callgraph(),
		envs                 :: dict(),
		fun_tab		     :: dict(),
		plt		     :: dialyzer_plt:plt(),
		opaques              :: [erl_types:erl_type()],
    races = dialyzer_races:<warning>new</warning>() :: dialyzer_races:races(),
    records = dict:<warning>new</warning>()  :: dict(),
		tree_map	     :: dict(),
		warning_mode = false :: boolean(),
		warnings = []        :: [dial_warning()],
		work                 :: {[_], set()},
		module               :: module(),
		behaviour_api_dict = [] :: dialyzer_behaviours:behaviour_api_dict()}).

%% Exported Types

-opaque state() :: #state{}.

-record(mnesia_select, {tab,tid,node,storage,cont,written=[],spec,type,orig}).
