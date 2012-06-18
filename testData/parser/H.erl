bind_pat_vars([Pat|PatLeft], [Type|TypeLeft], Acc, Map, State, Rev) ->
  ?debug("Binding pat: ~w to ~s\n", [cerl:type(Pat), format_type(Type, State)]),
  {NewMap, TypeOut} =
    case cerl:type(Pat) of
      alias ->
	AliasPat = cerl:alias_pat(Pat),
	Var = cerl:alias_var(Pat),
	Map1 = enter_subst(Var, AliasPat, Map),
	{Map2, [PatType]} = bind_pat_vars([AliasPat], [Type], [],
					  Map1, State, Rev),
	{enter_type(Var, PatType, Map2), PatType};
      binary ->
	%% Cannot bind the binary if we are in reverse match since
	%% binary patterns and binary construction are not symmetric.
	case Rev of
	  true -> {Map, t_bitstr()};
	  false ->
	    BinType = t_inf(t_bitstr(), Type),
	    case t_is_none(BinType) of
	      true -> bind_error([Pat], Type, t_none(), bind);
	      false ->
		Segs = cerl:binary_segments(Pat),
		{Map1, SegTypes} = bind_bin_segs(Segs, BinType, Map, State),
		{Map1, t_bitstr_concat(SegTypes)}
	    end
	end;
      cons ->
	Cons = t_inf(Type, t_cons()),
	case t_is_none(Cons) of
	  true ->
	    bind_opaque_pats(t_cons(), Type, Pat, Map, State, Rev);
	  false ->
	    {Map1, [HdType, TlType]} =
	      bind_pat_vars([cerl:cons_hd(Pat), cerl:cons_tl(Pat)],
			    [t_cons_hd(Cons), t_cons_tl(Cons)],
			    [], Map, State, Rev),
	    {Map1, t_cons(HdType, TlType)}
	end;
      literal ->
	Literal = literal_type(Pat),
	LiteralOrOpaque =
	  case t_opaque_match_atom(Literal, State#state.opaques) of
	    [Opaque] -> Opaque;
	    _ -> Literal
	  end,
	case t_is_none(t_inf(LiteralOrOpaque, Type)) of
	  true ->
	    bind_opaque_pats(Literal, Type, Pat, Map, State, Rev);
	  false -> {Map, LiteralOrOpaque}
	end;
      tuple ->
	Es = cerl:tuple_es(Pat),
	{TypedRecord, Prototype} =
	  case Es of
	    [] -> {false, t_tuple([])};
	    [Tag|Left] ->
	      case cerl:is_c_atom(Tag) of
		true ->
		  TagAtom = cerl:atom_val(Tag),
		  case state__lookup_record(TagAtom, length(Left), State) of
		    error -> {false, t_tuple(length(Es))};
		    {ok, Record} ->
		      [_Head|AnyTail] = [t_any() || _ <- Es],
		      UntypedRecord = t_tuple([t_atom(TagAtom)|AnyTail]),
		      {not erl_types:t_is_equal(Record, UntypedRecord), Record}
		  end;
		false -> {false, t_tuple(length(Es))}
	      end
	  end,
	Tuple = t_inf(Prototype, Type),
	case t_is_none(Tuple) of
	  true ->
	    bind_opaque_pats(Prototype, Type, Pat, Map, State, Rev);
	  false ->
	    SubTuples = t_tuple_subtypes(Tuple),
	    %% Need to call the top function to get the try-catch wrapper
	    Results =
	      case Rev of
		true ->
		  [bind_pat_vars_reverse(Es, t_tuple_args(SubTuple), [],
					 Map, State)
		   || SubTuple <- SubTuples];
		false ->
		  [bind_pat_vars(Es, t_tuple_args(SubTuple), [], Map, State)
		   || SubTuple <- SubTuples]
	      end,
	    case lists:keyfind(opaque, 2, Results) of
	      {error, opaque, _PatList, _Type, Opaque} ->
		bind_error([Pat], Tuple, Opaque, opaque);
	      false ->
		case [M || {M, _} <- Results, M =/= error] of
		  [] ->
		    case TypedRecord of
		      true -> bind_error([Pat], Tuple, Prototype, record);
		      false -> bind_error([Pat], Tuple, t_none(), bind)
		    end;
		  Maps ->
		    Map1 = join_maps(Maps, Map),
		    TupleType = t_sup([t_tuple(EsTypes)
				       || {M, EsTypes} <- Results, M =/= error]),
		    {Map1, TupleType}
		end
	    end
	end;
      values ->
	Es = cerl:values_es(Pat),
	{Map1, EsTypes} =
	  bind_pat_vars(Es, t_to_tlist(Type), [], Map, State, Rev),
	{Map1, t_product(EsTypes)};
      var ->
	Opaques = State#state.opaques,
	VarType1 =
	  case state__lookup_type_for_rec_var(Pat, State) of
	    error ->
	      LType = lookup_type(Pat, Map),
	      case t_opaque_match_record(LType, Opaques) of
		[Opaque] -> Opaque;
		_ ->
		  case t_opaque_match_atom(LType, Opaques) of
		    [Opaque] -> Opaque;
		    _ ->  LType
		  end
	      end;
	    {ok, RecType} -> RecType
	  end,
	%% Must do inf when binding args to pats. Vars in pats are fresh.
	VarType2 = t_inf(VarType1, Type),
	VarType3 =
	  case Opaques =/= [] of
	    true ->
	      case t_opaque_match_record(VarType2, Opaques) of
		[OpaqueRec] -> OpaqueRec;
		_ ->
		  case t_opaque_match_atom(VarType2, Opaques) of
		    [OpaqueAtom] -> OpaqueAtom;
		    _ -> VarType2
		  end
	      end;
	    false -> VarType2
	  end,
	case t_is_none(VarType3) of
	  true ->
	    case t_find_opaque_mismatch(VarType1, Type) of
	      {ok, T1, T2}  ->
		bind_error([Pat], T1, T2, opaque);
	      error ->
		bind_error([Pat], Type, t_none(), bind)
	    end;
	  false ->
	    Map1 = enter_type(Pat, VarType3, Map),
	    {Map1, VarType3}
	end;
      _Other ->
	%% Catch all is needed when binding args to pats
	?debug("Failed match for ~p\n", [_Other]),
	bind_error([Pat], Type, t_none(), bind)
    end,
  bind_pat_vars(PatLeft, TypeLeft, [TypeOut|Acc], NewMap, State, Rev);
bind_pat_vars([], [], Acc, Map, _State, _Rev) ->
  {Map, lists:reverse(Acc)}.

bind_bin_segs(BinSegs, BinType, Map, State) ->
  bind_bin_segs(BinSegs, BinType, [], Map, State).

bind_bin_segs([Seg|Segs], BinType, Acc, Map, State) ->
  Val = cerl:bitstr_val(Seg),
  SegType = cerl:concrete(cerl:bitstr_type(Seg)),
  UnitVal = cerl:concrete(cerl:bitstr_unit(Seg)),
  case cerl:bitstr_bitsize(Seg) of
    all ->
      binary = SegType, [] = Segs, %% just an assert
      T = t_inf(t_bitstr(UnitVal, 0), BinType),
      {Map1, [Type]} = bind_pat_vars([Val], [T], [], Map, State, false),
      bind_bin_segs(Segs, t_bitstr(0, 0), [Type|Acc], Map1, State);
    utf -> % XXX: possibly can be strengthened
      true = lists:member(SegType, [utf8, utf16, utf32]),
      {Map1, [_]} = bind_pat_vars([Val], [t_integer()], [], Map, State, false),
      Type = t_binary(),
      bind_bin_segs(Segs, BinType, [Type|Acc], Map1, State);
    BitSz when is_integer(BitSz) orelse BitSz =:= any ->
      Size = cerl:bitstr_size(Seg),
      {Map1, [SizeType]} =
	bind_pat_vars([Size], [t_non_neg_integer()], [], Map, State, false),
      Type =
	case t_number_vals(SizeType) of
	  [OneSize] -> t_bitstr(0, UnitVal * OneSize);
	  _ ->
	    MinSize = erl_types:number_min(SizeType),
	    t_bitstr(UnitVal, UnitVal * MinSize)
	end,
      ValConstr =
	case SegType of
	  binary -> Type; %% The same constraints as for the whole bitstr
	  float -> t_float();
	  integer ->
	    case t_number_vals(SizeType) of
	      unknown -> t_integer();
	      List ->
		SizeVal = lists:max(List),
		Flags = cerl:concrete(cerl:bitstr_flags(Seg)),
		N = SizeVal * UnitVal,
		case lists:member(signed, Flags) of
		  true -> t_from_range(-(1 bsl (N - 1)), 1 bsl (N - 1) - 1);
		  false -> t_from_range(0, 1 bsl N - 1)
		end
	    end
	end,
      {Map2, [_]} = bind_pat_vars([Val], [ValConstr], [], Map1, State, false),
      NewBinType = t_bitstr_match(Type, BinType),
      case t_is_none(NewBinType) of
	true -> bind_error([Seg], BinType, t_none(), bind);
	false -> bind_bin_segs(Segs, NewBinType, [Type|Acc], Map2, State)
      end
  end;
bind_bin_segs([], _BinType, Acc, Map, _State) ->
  {Map, lists:reverse(Acc)}.

bind_error(Pats, Type, OpaqueType, Error) ->
  throw({error, Error, Pats, Type, OpaqueType}).

bind_opaque_pats(GenType, Type, Pat, Map, State, Rev) ->
  case t_find_opaque_mismatch(GenType, Type) of
    {ok, T1, T2}  ->
      case lists:member(T2, State#state.opaques) of
	true ->
	  NewType = erl_types:t_struct_from_opaque(Type, [T2]),
	  {Map1, _} =
	    bind_pat_vars([Pat], [NewType], [], Map, State, Rev),
	  {Map1, T2};
	false -> bind_error([Pat], T1, T2, opaque)
      end;
    error -> bind_error([Pat], Type, t_none(), bind)
  end.

%%----------------------------------------
%% Guards
%%

bind_guard(Guard, Map, State) ->
  try bind_guard(Guard, Map, dict:new(), pos, State) of
    {Map1, _Type} -> Map1
  catch
    throw:{fail, Warning} -> {error, Warning};
    throw:{fatal_fail, Warning} -> {error, Warning}
  end.

bind_guard(Guard, Map, Env, Eval, State) ->
  ?debug("Handling ~w guard: ~s\n",
	 [Eval, cerl_prettypr:format(Guard, [{noann, true}])]),
  case cerl:type(Guard) of
    binary ->
      {Map, t_binary()};
    'case' ->
      Arg = cerl:case_arg(Guard),
      Clauses = cerl:case_clauses(Guard),
      bind_guard_case_clauses(Arg, Clauses, Map, Env, Eval, State);
    cons ->
      Hd = cerl:cons_hd(Guard),
      Tl = cerl:cons_tl(Guard),
      {Map1, HdType} = bind_guard(Hd, Map, Env, dont_know, State),
      {Map2, TlType} = bind_guard(Tl, Map1, Env, dont_know, State),
      {Map2, t_cons(HdType, TlType)};
    literal ->
      {Map, literal_type(Guard)};
    'try' ->
      Arg = cerl:try_arg(Guard),
      [Var] = cerl:try_vars(Guard),
      %%?debug("Storing: ~w\n", [Var]),
      NewEnv = dict:store(get_label(Var), Arg, Env),
      bind_guard(cerl:try_body(Guard), Map, NewEnv, Eval, State);
    tuple ->
      Es0 = cerl:tuple_es(Guard),
      {Map1, Es} = bind_guard_list(Es0, Map, Env, dont_know, State),
      {Map1, t_tuple(Es)};
    'let' ->
      Arg = cerl:let_arg(Guard),
      [Var] = cerl:let_vars(Guard),
      %%?debug("Storing: ~w\n", [Var]),
      NewEnv = dict:store(get_label(Var), Arg, Env),
      bind_guard(cerl:let_body(Guard), Map, NewEnv, Eval, State);
    values ->
      Es = cerl:values_es(Guard),
      List = [bind_guard(V, Map, Env, dont_know, State) || V <- Es],
      Type = t_product([T || {_, T} <- List]),
      {Map, Type};
    var ->
      ?debug("Looking for var(~w)...", [cerl_trees:get_label(Guard)]),
      case dict:find(get_label(Guard), Env) of
	error ->
	  ?debug("Did not find it\n", []),
	  Type = lookup_type(Guard, Map),
	  Constr =
	    case Eval of
	      pos -> t_atom(true);
	      neg -> t_atom(false);
	      dont_know -> Type
	    end,
	  Inf = t_inf(Constr, Type),
	  {enter_type(Guard, Inf, Map), Inf};
	{ok, Tree} ->
	  ?debug("Found it\n", []),
	  {Map1, Type} = bind_guard(Tree, Map, Env, Eval, State),
	  {enter_type(Guard, Type, Map1), Type}
      end;
    call ->
      handle_guard_call(Guard, Map, Env, Eval, State)
  end.

handle_guard_call(Guard, Map, Env, Eval, State) ->
  MFA = {cerl:atom_val(cerl:call_module(Guard)),
	 cerl:atom_val(cerl:call_name(Guard)),
	 cerl:call_arity(Guard)},
  case MFA of
    {erlang, F, 1} when F =:= is_atom; F =:= is_boolean;
			F =:= is_binary; F =:= is_bitstring;
			F =:= is_float; F =:= is_function;
			F =:= is_integer; F =:= is_list;
			F =:= is_number; F =:= is_pid; F =:= is_port;
			F =:= is_reference; F =:= is_tuple ->
      handle_guard_type_test(Guard, F, Map, Env, Eval, State);
    {erlang, is_function, 2} ->
      handle_guard_is_function(Guard, Map, Env, Eval, State);
    MFA when (MFA =:= {erlang, internal_is_record, 3}) or
	     (MFA =:= {erlang, is_record, 3}) ->
      handle_guard_is_record(Guard, Map, Env, Eval, State);
    {erlang, '=:=', 2} ->
      handle_guard_eqeq(Guard, Map, Env, Eval, State);
    {erlang, '==', 2} ->
      handle_guard_eq(Guard, Map, Env, Eval, State);
    {erlang, 'and', 2} ->
      handle_guard_and(Guard, Map, Env, Eval, State);
    {erlang, 'or', 2} ->
      handle_guard_or(Guard, Map, Env, Eval, State);
    {erlang, 'not', 1} ->
      handle_guard_not(Guard, Map, Env, Eval, State);
    {erlang, Comp, 2} when Comp =:= '<'; Comp =:= '=<';
                           Comp =:= '>'; Comp =:= '>=' ->
      handle_guard_comp(Guard, Comp, Map, Env, Eval, State);
    _ ->
      handle_guard_gen_fun(MFA, Guard, Map, Env, Eval, State)
  end.

handle_guard_gen_fun({M, F, A}, Guard, Map, Env, Eval, State) ->
  Args = cerl:call_args(Guard),
  {Map1, As0} = bind_guard_list(Args, Map, Env, dont_know, State),
  MapFun = fun(Type) ->
	       case lists:member(Type, State#state.opaques) of
		 true -> erl_types:t_opaque_structure(Type);
		 false -> Type
	       end
	   end,
  As = lists:map(MapFun, As0),
  Mode = case As =:= As0 of
	   true -> structured;
	   false -> opaque
	 end,
  BifRet = erl_bif_types:type(M, F, A, As),
  case t_is_none(BifRet) of
    true ->
      %% Is this an error-bif?
      case t_is_none(erl_bif_types:type(M, F, A)) of
	true -> signal_guard_fail(Eval, Guard, As, State);
	false -> signal_guard_fatal_fail(Eval, Guard, As, State)
      end;
    false ->
      BifArgs = case erl_bif_types:arg_types(M, F, A) of
		  unknown -> lists:duplicate(A, t_any());
		  List -> List
		end,
      Map2 = enter_type_lists(Args, t_inf_lists(BifArgs, As0, Mode), Map1),
      Ret =
	case Eval of
	  pos -> t_inf(t_atom(true), BifRet);
	  neg -> t_inf(t_atom(false), BifRet);
	  dont_know -> BifRet
	end,
      case t_is_none(Ret) of
	true ->
	  case Eval =:= pos of
	    true -> signal_guard_fail(Eval, Guard, As, State);
	    false -> throw({fail, none})
	  end;
	false -> {Map2, Ret}
      end
  end.

handle_guard_type_test(Guard, F, Map, Env, Eval, State) ->
  [Arg] = cerl:call_args(Guard),
  {Map1, ArgType} = bind_guard(Arg, Map, Env, dont_know, State),
  case bind_type_test(Eval, F, ArgType, State) of
    error ->
      ?debug("Type test: ~w failed\n", [F]),
      signal_guard_fail(Eval, Guard, [ArgType], State);
    {ok, NewArgType, Ret} ->
      ?debug("Type test: ~w succeeded, NewType: ~s, Ret: ~s\n",
	     [F, t_to_string(NewArgType), t_to_string(Ret)]),
      {enter_type(Arg, NewArgType, Map1), Ret}
  end.

bind_type_test(Eval, TypeTest, ArgType, State) ->
  Type = case TypeTest of
	   is_atom -> t_atom();
	   is_boolean -> t_boolean();
	   is_binary -> t_binary();
	   is_bitstring -> t_bitstr();
	   is_float -> t_float();
	   is_function -> t_fun();
	   is_integer -> t_integer();
	   is_list -> t_maybe_improper_list();
	   is_number -> t_number();
	   is_pid -> t_pid();
	   is_port -> t_port();
	   is_reference -> t_reference();
	   is_tuple -> t_tuple()
	 end,
  Mode = determine_mode(ArgType, State#state.opaques),
  case Eval of
    pos ->
      Inf = t_inf(Type, ArgType, Mode),
      case t_is_none(Inf) of
	true -> error;
	false -> {ok, Inf, t_atom(true)}
      end;
    neg ->
      case Mode of
	opaque ->
	  Struct = erl_types:t_opaque_structure(ArgType),
	  case t_is_none(t_subtract(Struct, Type)) of
	    true -> error;
	    false  -> {ok, ArgType, t_atom(false)}
	  end;
	structured ->
	  Sub = t_subtract(ArgType, Type),
	  case t_is_none(Sub) of
	    true -> error;
	    false -> {ok, Sub, t_atom(false)}
	  end
	end;
    dont_know ->
      {ok, ArgType, t_boolean()}
  end.

handle_guard_comp(Guard, Comp, Map, Env, Eval, State) ->
  Args = cerl:call_args(Guard),
  [Arg1, Arg2] = Args,
  {Map1, ArgTypes} = bind_guard_list(Args, Map, Env, dont_know, State),
  [Type1, Type2] = ArgTypes,
  IsInt1 = t_is_integer(Type1),
  IsInt2 = t_is_integer(Type2),
  case {cerl:type(Arg1), cerl:type(Arg2)} of
    {literal, literal} ->
      case erlang:Comp(cerl:concrete(Arg1), cerl:concrete(Arg2)) of
	true  when Eval =:= pos ->       {Map, t_atom(true)};
	true  when Eval =:= dont_know -> {Map, t_atom(true)};
	true  when Eval =:= neg ->       {Map, t_atom(true)};
	false when Eval =:= pos ->
	  signal_guard_fail(Eval, Guard, ArgTypes, State);
	false when Eval =:= dont_know -> {Map, t_atom(false)};
	false when Eval =:= neg ->       {Map, t_atom(false)}
      end;
    {literal, var} when IsInt1 andalso IsInt2 andalso (Eval =:= pos) ->
      case bind_comp_literal_var(Arg1, Arg2, Type2, Comp, Map1) of
	error -> signal_guard_fail(Eval, Guard, ArgTypes, State);
	{ok, NewMap} -> {NewMap, t_atom(true)}
      end;
    {var, literal} when IsInt1 andalso IsInt2 andalso (Eval =:= pos) ->
      case bind_comp_literal_var(Arg2, Arg1, Type1, invert_comp(Comp), Map1) of
	error -> signal_guard_fail(Eval, Guard, ArgTypes, State);
	{ok, NewMap} -> {NewMap, t_atom(true)}
      end;
    {_, _} ->
      handle_guard_gen_fun({erlang, Comp, 2}, Guard, Map, Env, Eval, State)
  end.

invert_comp('=<') -> '>=';
invert_comp('<')  -> '>';
invert_comp('>=') -> '=<';
invert_comp('>')  -> '<'.

bind_comp_literal_var(Lit, Var, VarType, CompOp, Map) ->
  LitVal = cerl:concrete(Lit),
  NewVarType =
    case t_number_vals(VarType) of
      unknown ->
	Range =
	  case CompOp of
	    '=<' -> t_from_range(LitVal, pos_inf);
	    '<'  -> t_from_range(LitVal + 1, pos_inf);
	    '>=' -> t_from_range(neg_inf, LitVal);
	    '>'  -> t_from_range(neg_inf, LitVal - 1)
	  end,
	t_inf(Range, VarType);
      NumberVals ->
	NewNumberVals = [X || X <- NumberVals, erlang:CompOp(LitVal, X)],
	t_integers(NewNumberVals)
    end,
  case t_is_none(NewVarType) of
    true -> error;
    false -> {ok, enter_type(Var, NewVarType, Map)}
  end.

handle_guard_is_function(Guard, Map, Env, Eval, State) ->
  Args = cerl:call_args(Guard),
  {Map1, ArgTypes0} = bind_guard_list(Args, Map, Env, dont_know, State),
  [FunType0, ArityType0] = ArgTypes0,
  ArityType = t_inf(ArityType0, t_integer()),
  case t_is_none(ArityType) of
    true -> signal_guard_fail(Eval, Guard, ArgTypes0, State);
    false ->
      FunTypeConstr =
	case t_number_vals(ArityType) of
	  unknown -> t_fun();
	  Vals ->
	    t_sup([t_fun(lists:duplicate(X, t_any()), t_any()) || X <- Vals])
	end,
      FunType = t_inf(FunType0, FunTypeConstr),
      case t_is_none(FunType) of
	true ->
	  case Eval of
	    pos -> signal_guard_fail(Eval, Guard, ArgTypes0, State);
	    neg -> {Map1, t_atom(false)};
	    dont_know -> {Map1, t_atom(false)}
	  end;
	false ->
	  case Eval of
	    pos -> {enter_type_lists(Args, [FunType, ArityType], Map1),
		    t_atom(true)};
	    neg -> {Map1, t_atom(false)};
	    dont_know -> {Map1, t_boolean()}
	  end
      end
  end.

handle_guard_is_record(Guard, Map, Env, Eval, State) ->
  Args = cerl:call_args(Guard),
  [Rec, Tag0, Arity0] = Args,
  Tag = cerl:atom_val(Tag0),
  Arity = cerl:int_val(Arity0),
  {Map1, RecType} = bind_guard(Rec, Map, Env, dont_know, State),
  ArityMin1 = Arity - 1,
  TupleType =
    case state__lookup_record(Tag, ArityMin1, State) of
      error -> t_tuple([t_atom(Tag)|lists:duplicate(ArityMin1, t_any())]);
      {ok, Prototype} -> Prototype
    end,
  Mode = determine_mode(RecType, State#state.opaques),
  NewTupleType =
    case t_opaque_match_record(TupleType, State#state.opaques) of
      [Opaque] -> Opaque;
      _ -> TupleType
    end,
  Type = t_inf(NewTupleType, RecType, Mode),
  case t_is_none(Type) of
    true ->
      case Eval of
	pos -> signal_guard_fail(Eval, Guard,
				 [RecType, t_from_term(Tag),
				  t_from_term(Arity)],
				 State);
	neg -> {Map1, t_atom(false)};
	dont_know -> {Map1, t_atom(false)}
      end;
    false ->
      case Eval of
	pos -> {enter_type(Rec, Type, Map1), t_atom(true)};
	neg -> {Map1, t_atom(false)};
	dont_know -> {Map1, t_boolean()}
      end
  end.

handle_guard_eq(Guard, Map, Env, Eval, State) ->
  [Arg1, Arg2] = cerl:call_args(Guard),
  case {cerl:type(Arg1), cerl:type(Arg2)} of
    {literal, literal} ->
      case cerl:concrete(Arg1) =:= cerl:concrete(Arg2) of
	true ->
	  if
	    Eval =:= pos -> {Map, t_atom(true)};
	    Eval =:= neg ->
	      ArgTypes = [t_from_term(cerl:concrete(Arg1)),
			  t_from_term(cerl:concrete(Arg2))],
	      signal_guard_fail(Eval, Guard, ArgTypes, State);
	    Eval =:= dont_know -> {Map, t_atom(true)}
	  end;
	false ->
	  if
	    Eval =:= neg -> {Map, t_atom(false)};
	    Eval =:= dont_know -> {Map, t_atom(false)};
	    Eval =:= pos ->
	      ArgTypes = [t_from_term(cerl:concrete(Arg1)),
			  t_from_term(cerl:concrete(Arg2))],
	      signal_guard_fail(Eval, Guard, ArgTypes, State)
	  end
      end;
    {literal, _} when Eval =:= pos ->
      case cerl:concrete(Arg1) of
	Atom when is_atom(Atom) ->
	  bind_eqeq_guard_lit_other(Guard, Arg1, Arg2, Map, Env, State);
	[] ->
	  bind_eqeq_guard_lit_other(Guard, Arg1, Arg2, Map, Env, State);
	_ ->
	  bind_eq_guard(Guard, Arg1, Arg2, Map, Env, Eval, State)
      end;
    {_, literal} when Eval =:= pos ->
      case cerl:concrete(Arg2) of
	Atom when is_atom(Atom) ->
	  bind_eqeq_guard_lit_other(Guard, Arg2, Arg1, Map, Env, State);
	[] ->
	  bind_eqeq_guard_lit_other(Guard, Arg2, Arg1, Map, Env, State);
	_ ->
	  bind_eq_guard(Guard, Arg1, Arg2, Map, Env, Eval, State)
      end;
    {_, _} ->
      bind_eq_guard(Guard, Arg1, Arg2, Map, Env, Eval, State)
  end.

bind_eq_guard(Guard, Arg1, Arg2, Map, Env, Eval, State) ->
  {Map1, Type1} = bind_guard(Arg1, Map, Env, dont_know, State),
  {Map2, Type2} = bind_guard(Arg2, Map1, Env, dont_know, State),
  case (t_is_nil(Type1) orelse t_is_nil(Type2) orelse
	t_is_atom(Type1) orelse t_is_atom(Type2)) of
    true -> bind_eqeq_guard(Guard, Arg1, Arg2, Map, Env, Eval, State);
    false ->
      case Eval of
	pos -> {Map2, t_atom(true)};
	neg -> {Map2, t_atom(false)};
	dont_know -> {Map2, t_boolean()}
      end
  end.

handle_guard_eqeq(Guard, Map, Env, Eval, State) ->
  [Arg1, Arg2] = cerl:call_args(Guard),
  case {cerl:type(Arg1), cerl:type(Arg2)} of
    {literal, literal} ->
      case cerl:concrete(Arg1) =:= cerl:concrete(Arg2) of
	true ->
	  if Eval =:= neg ->
	      ArgTypes = [t_from_term(cerl:concrete(Arg1)),
			  t_from_term(cerl:concrete(Arg2))],
	      signal_guard_fail(Eval, Guard, ArgTypes, State);
	     Eval =:= pos -> {Map, t_atom(true)};
	     Eval =:= dont_know -> {Map, t_atom(true)}
	  end;
	false ->
	  if Eval =:= neg -> {Map, t_atom(false)};
	     Eval =:= dont_know -> {Map, t_atom(false)};
	     Eval =:= pos ->
	      ArgTypes = [t_from_term(cerl:concrete(Arg1)),
			  t_from_term(cerl:concrete(Arg2))],
	      signal_guard_fail(Eval, Guard, ArgTypes, State)
	  end
      end;
    {literal, _} when Eval =:= pos ->
      bind_eqeq_guard_lit_other(Guard, Arg1, Arg2, Map, Env, State);
    {_, literal} when Eval =:= pos ->
      bind_eqeq_guard_lit_other(Guard, Arg2, Arg1, Map, Env, State);
    {_, _} ->
      bind_eqeq_guard(Guard, Arg1, Arg2, Map, Env, Eval, State)
  end.

bind_eqeq_guard(Guard, Arg1, Arg2, Map, Env, Eval, State) ->
  {Map1, Type1} = bind_guard(Arg1, Map, Env, dont_know, State),
  {Map2, Type2} = bind_guard(Arg2, Map1, Env, dont_know, State),
  ?debug("Types are:~s =:= ~s\n", [t_to_string(Type1),
				   t_to_string(Type2)]),
  Inf = t_inf(Type1, Type2),
  case t_is_none(Inf) of
    true ->
      case Eval of
	neg -> {Map2, t_atom(false)};
	dont_know -> {Map2, t_atom(false)};
	pos -> signal_guard_fail(Eval, Guard, [Type1, Type2], State)
      end;
    false ->
      case Eval of
	pos ->
	  case {cerl:type(Arg1), cerl:type(Arg2)} of
	    {var, var} ->
	      Map3 = enter_subst(Arg1, Arg2, Map2),
	      Map4 = enter_type(Arg2, Inf, Map3),
	      {Map4, t_atom(true)};
	    {var, _} ->
	      Map3 = enter_type(Arg1, Inf, Map2),
	      {Map3, t_atom(true)};
	    {_, var} ->
	      Map3 = enter_type(Arg2, Inf, Map2),
	      {Map3, t_atom(true)};
	    {_, _} ->
	      {Map2, t_atom(true)}
	  end;
	neg ->
	  {Map2, t_atom(false)};
	dont_know ->
	  {Map2, t_boolean()}
      end
  end.

bind_eqeq_guard_lit_other(Guard, Arg1, Arg2, Map, Env, State) ->
  Eval = dont_know,
  case cerl:concrete(Arg1) of
    true ->
      {_, Type} = MT = bind_guard(Arg2, Map, Env, pos, State),
      case t_is_atom(true, Type) of
	true -> MT;
	false ->
	  {_, Type0} = bind_guard(Arg2, Map, Env, Eval, State),
	  signal_guard_fail(Eval, Guard, [Type0, t_atom(true)], State)
      end;
    false ->
      {Map1, Type} = bind_guard(Arg2, Map, Env, neg, State),
      case t_is_atom(false, Type) of
	true -> {Map1, t_atom(true)};
	false ->
	  {_, Type0} = bind_guard(Arg2, Map, Env, Eval, State),
	  signal_guard_fail(Eval, Guard, [Type0, t_atom(false)], State)
      end;
    Term ->
      LitType = t_from_term(Term),
      {Map1, Type} = bind_guard(Arg2, Map, Env, Eval, State),
      case t_is_subtype(LitType, Type) of
	false -> signal_guard_fail(Eval, Guard, [Type, LitType], State);
	true ->
	  case cerl:is_c_var(Arg2) of
	    true -> {enter_type(Arg2, LitType, Map1), t_atom(true)};
	    false -> {Map1, t_atom(true)}
	  end
      end
  end.

handle_guard_and(Guard, Map, Env, Eval, State) ->
  [Arg1, Arg2] = cerl:call_args(Guard),
  case Eval of
    pos ->
      {Map1, Type1} = bind_guard(Arg1, Map, Env, Eval, State),
      case t_is_atom(true, Type1) of
	false -> signal_guard_fail(Eval, Guard, [Type1, t_any()], State);
	true ->
	  {Map2, Type2} = bind_guard(Arg2, Map1, Env, Eval, State),
	  case t_is_atom(true, Type2) of
	    false -> signal_guard_fail(Eval, Guard, [Type1, Type2], State);
	    true -> {Map2, t_atom(true)}
	  end
      end;
    neg ->
      {Map1, Type1} =
	try bind_guard(Arg1, Map, Env, neg, State)
	catch throw:{fail, _} -> bind_guard(Arg2, Map, Env, pos, State)
	end,
      {Map2, Type2} =
	try bind_guard(Arg2, Map, Env, neg, State)
	catch throw:{fail, _} -> bind_guard(Arg1, Map, Env, pos, State)
	end,
      case t_is_atom(false, Type1) orelse t_is_atom(false, Type2) of
	true -> {join_maps([Map1, Map2], Map), t_atom(false)};
	false -> signal_guard_fail(Eval, Guard, [Type1, Type2], State)
      end;
    dont_know ->
      {Map1, Type1} = bind_guard(Arg1, Map, Env, dont_know, State),
      {Map2, Type2} = bind_guard(Arg2, Map, Env, dont_know, State),
      Bool1 = t_inf(Type1, t_boolean()),
      Bool2 = t_inf(Type2, t_boolean()),
      case t_is_none(Bool1) orelse t_is_none(Bool2) of
	true -> throw({fatal_fail, none});
	false ->
	  NewMap = join_maps([Map1, Map2], Map),
	  NewType =
	    case {t_atom_vals(Bool1), t_atom_vals(Bool2)} of
	      {['true'] , ['true'] } -> t_atom(true);
	      {['false'], _        } -> t_atom(false);
	      {_        , ['false']} -> t_atom(false);
	      {_        , _        } -> t_boolean()
	    end,
	  {NewMap, NewType}
      end
  end.

handle_guard_or(Guard, Map, Env, Eval, State) ->
  [Arg1, Arg2] = cerl:call_args(Guard),
  case Eval of
    pos ->
      {Map1, Bool1} =
	try bind_guard(Arg1, Map, Env, pos, State)
	catch
	  throw:{fail,_} -> bind_guard(Arg1, Map, Env, dont_know, State)
	end,
      {Map2, Bool2} =
	try bind_guard(Arg2, Map, Env, pos, State)
	catch
	  throw:{fail,_} -> bind_guard(Arg2, Map, Env, dont_know, State)
	end,
      case ((t_is_atom(true, Bool1) andalso t_is_boolean(Bool2))
	    orelse
	    (t_is_atom(true, Bool2) andalso t_is_boolean(Bool1))) of
	true -> {join_maps([Map1, Map2], Map), t_atom(true)};
	false -> signal_guard_fail(Eval, Guard, [Bool1, Bool2], State)
      end;
    neg ->
      {Map1, Type1} = bind_guard(Arg1, Map, Env, neg, State),
      case t_is_atom(false, Type1) of
	false -> signal_guard_fail(Eval, Guard, [Type1, t_any()], State);
	true ->
	  {Map2, Type2} = bind_guard(Arg2, Map1, Env, neg, State),
	  case t_is_atom(false, Type2) of
	    false -> signal_guard_fail(Eval, Guard, [Type1, Type2], State);
	    true -> {Map2, t_atom(false)}
	  end
      end;
    dont_know ->
      {Map1, Type1} = bind_guard(Arg1, Map, Env, dont_know, State),
      {Map2, Type2} = bind_guard(Arg2, Map, Env, dont_know, State),
      Bool1 = t_inf(Type1, t_boolean()),
      Bool2 = t_inf(Type2, t_boolean()),
      case t_is_none(Bool1) orelse t_is_none(Bool2) of
	true -> throw({fatal_fail, none});
	false ->
	  NewMap = join_maps([Map1, Map2], Map),
	  NewType =
	    case {t_atom_vals(Bool1), t_atom_vals(Bool2)} of
	      {['false'], ['false']} -> t_atom(false);
	      {['true'] , _        } -> t_atom(true);
	      {_        , ['true'] } -> t_atom(true);
	      {_        , _        } -> t_boolean()
	    end,
	  {NewMap, NewType}
      end
  end.

handle_guard_not(Guard, Map, Env, Eval, State) ->
  [Arg] = cerl:call_args(Guard),
  case Eval of
    neg ->
      {Map1, Type} = bind_guard(Arg, Map, Env, pos, State),
      case t_is_atom(true, Type) of
	true -> {Map1, t_atom(false)};
	false ->
	  {_, Type0} = bind_guard(Arg, Map, Env, Eval, State),
	  signal_guard_fail(Eval, Guard, [Type0], State)
      end;
    pos ->
      {Map1, Type} = bind_guard(Arg, Map, Env, neg, State),
      case t_is_atom(false, Type) of
	true -> {Map1, t_atom(true)};
	false ->
	  {_, Type0} = bind_guard(Arg, Map, Env, Eval, State),
	  signal_guard_fail(Eval, Guard, [Type0], State)
      end;
    dont_know ->
      {Map1, Type} = bind_guard(Arg, Map, Env, dont_know, State),
      Bool = t_inf(Type, t_boolean()),
      case t_is_none(Bool) of
	true -> throw({fatal_fail, none});
	false ->
	  case t_atom_vals(Bool) of
	    ['true'] -> {Map1, t_atom(false)};
	    ['false'] -> {Map1, t_atom(true)};
	    [_, _] -> {Map1, Bool}
	  end
      end
  end.

bind_guard_list(Guards, Map, Env, Eval, State) ->
  bind_guard_list(Guards, Map, Env, Eval, State, []).

bind_guard_list([G|Gs], Map, Env, Eval, State, Acc) ->
  {Map1, T} = bind_guard(G, Map, Env, Eval, State),
  bind_guard_list(Gs, Map1, Env, Eval, State, [T|Acc]);
bind_guard_list([], Map, _Env, _Eval, _State, Acc) ->
  {Map, lists:reverse(Acc)}.

%-type eval() :: 'pos' | 'neg' | 'dont_know'.

-spec signal_guard_fail(eval(), cerl:c_call(), [erl_types:erl_type()],
			state()) -> no_return().

signal_guard_fail(Eval, Guard, ArgTypes, State) ->
  Args = cerl:call_args(Guard),
  F = cerl:atom_val(cerl:call_name(Guard)),
  MFA = {cerl:atom_val(cerl:call_module(Guard)), F, length(Args)},
  Msg =
    case is_infix_op(MFA) of
      true ->
	[ArgType1, ArgType2] = ArgTypes,
	[Arg1, Arg2] = Args,
	Kind = 
	  case Eval of
	    neg -> neg_guard_fail;
	    pos -> guard_fail;
	    dont_know -> guard_fail
	  end,
	{Kind, [format_args_1([Arg1], [ArgType1], State),
		atom_to_list(F),
		format_args_1([Arg2], [ArgType2], State)]};
      false ->
	mk_guard_msg(Eval, F, Args, ArgTypes, State)
    end,
  throw({fail, {Guard, Msg}}).

is_infix_op({erlang, '=:=', 2}) -> true;
is_infix_op({erlang, '==', 2}) -> true;
is_infix_op({erlang, '=/=', 2}) -> true;
is_infix_op({erlang, '=/', 2}) -> true;
is_infix_op({erlang, '<', 2}) -> true;
is_infix_op({erlang, '=<', 2}) -> true;
is_infix_op({erlang, '>', 2}) -> true;
is_infix_op({erlang, '>=', 2}) -> true;
is_infix_op({M, F, A}) when is_atom(M), is_atom(F),
			    is_integer(A), 0 =< A, A =< 255 -> false.

-spec signal_guard_fatal_fail(eval(), cerl:c_call(), [erl_types:erl_type()],
			      state()) -> no_return().

signal_guard_fatal_fail(Eval, Guard, ArgTypes, State) ->
  Args = cerl:call_args(Guard),
  F = cerl:atom_val(cerl:call_name(Guard)),
  Msg = mk_guard_msg(Eval, F, Args, ArgTypes, State),
  throw({fatal_fail, {Guard, Msg}}).

mk_guard_msg(Eval, F, Args, ArgTypes, State) ->
  FArgs = [F, format_args(Args, ArgTypes, State)],
  case any_has_opaque_subtype(ArgTypes) of
    true -> {opaque_guard, FArgs};
    false ->
      case Eval of
	neg -> {neg_guard_fail, FArgs};
	pos -> {guard_fail, FArgs};
	dont_know -> {guard_fail, FArgs}
      end
  end.

bind_guard_case_clauses(Arg, Clauses, Map, Env, Eval, State) ->
  Clauses1 = filter_fail_clauses(Clauses),
  {GenMap, GenArgType} = bind_guard(Arg, Map, Env, dont_know, State),
  bind_guard_case_clauses(GenArgType, GenMap, Arg, Clauses1, Map, Env, Eval,
			  t_none(), [], State).

filter_fail_clauses([Clause|Left]) ->
  case (cerl:clause_pats(Clause) =:= []) of
    true ->
      Body = cerl:clause_body(Clause),
      case cerl:is_literal(Body) andalso (cerl:concrete(Body) =:= fail) of
	true -> filter_fail_clauses(Left);
	false -> [Clause|filter_fail_clauses(Left)]
      end;
    false ->
      [Clause|filter_fail_clauses(Left)]
  end;
filter_fail_clauses([]) ->
  [].

bind_guard_case_clauses(GenArgType, GenMap, ArgExpr, [Clause|Left],
			Map, Env, Eval, AccType, AccMaps, State) ->
  Pats = cerl:clause_pats(Clause),
  {NewMap0, ArgType} =
    case Pats of
      [Pat] ->
	case cerl:is_literal(Pat) of
	  true ->
	    try
	      case cerl:concrete(Pat) of
		true -> bind_guard(ArgExpr, Map, Env, pos, State);
		false -> bind_guard(ArgExpr, Map, Env, neg, State);
		_ -> {GenMap, GenArgType}
	      end
	    catch
	      throw:{fail, _} -> {none, GenArgType}
	    end;
	  false ->
	    {GenMap, GenArgType}
	end;
      _ -> {GenMap, GenArgType}
    end,
  NewMap1 =
    case Pats =:= [] of
      true -> NewMap0;
      false ->
	case t_is_none(ArgType) of
	  true -> none;
	  false ->
	    ArgTypes = case t_is_any(ArgType) of
			 true -> Any = t_any(), [Any || _ <- Pats];
			 false -> t_to_tlist(ArgType)
		       end,
	    case bind_pat_vars(Pats, ArgTypes, [], NewMap0, State) of
	      {error, _, _, _, _} -> none;
	      {PatMap, _PatTypes} -> PatMap
	    end
	end
    end,
  Guard = cerl:clause_guard(Clause),
  GenPatType = dialyzer_typesig:get_safe_underapprox(Pats, Guard),
  NewGenArgType = t_subtract(GenArgType, GenPatType),
  case (NewMap1 =:= none) orelse t_is_none(GenArgType) of
    true ->
      bind_guard_case_clauses(NewGenArgType, GenMap, ArgExpr, Left, Map, Env,
			      Eval, AccType, AccMaps, State);
    false ->
      {NewAccType, NewAccMaps} =
	try
	  {NewMap2, GuardType} = bind_guard(Guard, NewMap1, Env, pos, State),
	  case t_is_none(t_inf(t_atom(true), GuardType)) of
	    true -> throw({fail, none});
	    false -> ok
	  end,
	  {NewMap3, CType} = bind_guard(cerl:clause_body(Clause), NewMap2,
					Env, Eval, State),
	  case Eval of
	    pos ->
	      case t_is_atom(true, CType) of
		true -> ok;
		false -> throw({fail, none})
	      end;
	    neg ->
	      case t_is_atom(false, CType) of
		true -> ok;
		false -> throw({fail, none})
	      end;
	    dont_know ->
	      ok
	  end,
	  {t_sup(AccType, CType), [NewMap3|AccMaps]}
	catch
	  throw:{fail, _What} -> {AccType, AccMaps}
	end,
      bind_guard_case_clauses(NewGenArgType, GenMap, ArgExpr, Left, Map, Env,
			      Eval, NewAccType, NewAccMaps, State)
  end;
bind_guard_case_clauses(_GenArgType, _GenMap, _ArgExpr, [], Map, _Env, _Eval,
			AccType, AccMaps, _State) ->
  case t_is_none(AccType) of
    true -> throw({fail, none});
    false -> {join_maps(AccMaps, Map), AccType}
  end.

%%% ===========================================================================
%%%
%%%  Maps and types.
%%%
%%% ===========================================================================

map__new() ->
  {dict:new(), dict:new()}.

join_maps(Maps, MapOut) ->
  {Map, Subst} = MapOut,
  Keys = ordsets:from_list(dict:fetch_keys(Map) ++ dict:fetch_keys(Subst)),
  join_maps(Keys, Maps, MapOut).

join_maps([Key|Left], Maps, MapOut) ->
  Type = join_maps_one_key(Maps, Key, t_none()),
  case t_is_equal(lookup_type(Key, MapOut), Type) of
    true ->  join_maps(Left, Maps, MapOut);
    false -> join_maps(Left, Maps, enter_type(Key, Type, MapOut))
  end;
join_maps([], _Maps, MapOut) ->
  MapOut.

join_maps_one_key([Map|Left], Key, AccType) ->
  case t_is_any(AccType) of
    true ->
      %% We can stop here
      AccType;
    false ->
      join_maps_one_key(Left, Key, t_sup(lookup_type(Key, Map), AccType))
  end;
join_maps_one_key([], _Key, AccType) ->
  AccType.

enter_type_lists([Key|KeyTail], [Val|ValTail], Map) ->
  Map1 = enter_type(Key, Val, Map),
  enter_type_lists(KeyTail, ValTail, Map1);
enter_type_lists([], [], Map) ->
  Map.

enter_type_list([{Key, Val}|Left], Map) ->
  Map1 = enter_type(Key, Val, Map),
  enter_type_list(Left, Map1);
enter_type_list([], Map) ->
  Map.

enter_type(Key, Val, {Map, Subst} = MS) ->
  case cerl:is_literal(Key) of
    true -> MS;
    false ->
      case cerl:is_c_values(Key) of
	true ->
	  Keys = cerl:values_es(Key),
	  case t_is_any(Val) orelse t_is_none(Val) of
	    true ->
	      enter_type_lists(Keys, [Val || _ <- Keys], MS);
	    false ->
	      enter_type_lists(cerl:values_es(Key), t_to_tlist(Val), MS)
	  end;
	false ->
	  KeyLabel = get_label(Key),
	  case dict:find(KeyLabel, Subst) of
	    {ok, NewKey} ->
	      ?debug("Binding ~p to ~p\n", [KeyLabel, NewKey]),
	      enter_type(NewKey, Val, MS);
	    error ->
	      ?debug("Entering ~p :: ~s\n", [KeyLabel, t_to_string(Val)]),
	      case dict:find(KeyLabel, Map) of
		{ok, Val} -> MS;
		{ok, _OldVal} -> {dict:store(KeyLabel, Val, Map), Subst};
		error -> {dict:store(KeyLabel, Val, Map), Subst}
	      end
	  end
      end
  end.

enter_subst(Key, Val, {Map, Subst} = MS) ->
  KeyLabel = get_label(Key),
  case cerl:is_literal(Val) of
    true ->
      NewMap = dict:store(KeyLabel, literal_type(Val), Map),
      {NewMap, Subst};
    false ->
      case cerl:is_c_var(Val) of
	false -> MS;
	true ->
	  ValLabel = get_label(Val),
	  case dict:find(ValLabel, Subst) of
	    {ok, NewVal} ->
	      enter_subst(Key, NewVal, MS);
	    error ->
	      if KeyLabel =:= ValLabel -> MS;
		 true ->
		  ?debug("Subst: storing ~p = ~p\n", [KeyLabel, ValLabel]),
		  NewSubst = dict:store(KeyLabel, ValLabel, Subst),
		  {Map, NewSubst}
	      end
	  end
      end
  end.

lookup_type(Key, {Map, Subst}) ->
  lookup(Key, Map, Subst, t_none()).

lookup(Key, Map, Subst, AnyNone) ->
  case cerl:is_literal(Key) of
    true -> literal_type(Key);
    false ->
      Label = get_label(Key),
      case dict:find(Label, Subst) of
	{ok, NewKey} -> lookup(NewKey, Map, Subst, AnyNone);
	error ->
	  case dict:find(Label, Map) of
	    {ok, Val} -> Val;
	    error -> AnyNone
	  end
      end
  end.

lookup_fun_sig(Fun, Callgraph, Plt) ->
  MFAorLabel =
    case dialyzer_callgraph:lookup_name(Fun, Callgraph) of
      error -> Fun;
      {ok, MFA} -> MFA
    end,
  dialyzer_plt:lookup(Plt, MFAorLabel).

literal_type(Lit) ->
  t_from_term(cerl:concrete(Lit)).

mark_as_fresh([Tree|Left], Map) ->
  SubTrees1 = lists:append(cerl:subtrees(Tree)),
  {SubTrees2, Map1} =
    case cerl:type(Tree) of
      bitstr ->
	%% The Size field is not fresh.
	{SubTrees1 -- [cerl:bitstr_size(Tree)], Map};
      var ->
	{SubTrees1, enter_type(Tree, t_any(), Map)};
      _ ->
	{SubTrees1, Map}
    end,
  mark_as_fresh(SubTrees2 ++ Left, Map1);
mark_as_fresh([], Map) ->
  Map.

-ifdef(DEBUG).
debug_pp_map(Map = {Map0, _Subst}) ->
  Keys = dict:fetch_keys(Map0),
  io:format("Map:\n", []),
  lists:foreach(fun (Key) ->
		    io:format("\t~w :: ~s\n",
			      [Key, t_to_string(lookup_type(Key, Map))])
		end, Keys),
  ok.
-else.
debug_pp_map(_Map) -> ok.
-endif.

%%% ===========================================================================
%%%
%%%  Utilities
%%%
%%% ===========================================================================

get_label(L) when is_integer(L) ->
  L;
get_label(T) ->
  cerl_trees:get_label(T).

t_is_simple(ArgType) ->
  t_is_atom(ArgType) orelse t_is_number(ArgType) orelse t_is_port(ArgType)
    orelse t_is_pid(ArgType) orelse t_is_reference(ArgType)
    orelse t_is_nil(ArgType).

%% t_is_structured(ArgType) ->
%%   case t_is_nil(ArgType) of
%%     true -> false;
%%     false ->
%%       SType = t_inf(t_sup([t_list(), t_tuple(), t_binary()]), ArgType),
%%       t_is_equal(ArgType, SType)
%%   end.

is_call_to_send(Tree) ->
  case cerl:is_c_call(Tree) of
    false -> false;
    true ->
      Mod = cerl:call_module(Tree),
      Name = cerl:call_name(Tree),
      Arity = cerl:call_arity(Tree),
      cerl:is_c_atom(Mod)
	andalso cerl:is_c_atom(Name)
	andalso (cerl:atom_val(Name) =:= '!')
	andalso (cerl:atom_val(Mod) =:= erlang)
	andalso (Arity =:= 2)
  end.

any_opaque(Ts) ->
  lists:any(fun erl_types:t_is_opaque/1, Ts).

any_has_opaque_subtype(Ts) ->
  lists:any(fun erl_types:t_has_opaque_subtype/1, Ts).

filter_match_fail([Clause] = Cls) ->
  Body = cerl:clause_body(Clause),
  case cerl:type(Body) of
    primop ->
      case cerl:atom_val(cerl:primop_name(Body)) of
	match_fail -> [];
	raise -> [];
	_ -> Cls
      end;
    _ -> Cls
  end;
filter_match_fail([H|T]) ->
  [H|filter_match_fail(T)];
filter_match_fail([]) ->
  %% This can actually happen, for example in
  %%      receive after 1 -> ok end
  [].

determine_mode(Type, Opaques) ->
  case lists:member(Type, Opaques) of
    true  -> opaque;
    false -> structured
  end.

%%% ===========================================================================
%%%
%%%  The State.
%%%
%%% ===========================================================================

state__new(Callgraph, Tree, Plt, Module, Records, BehaviourTranslations) ->
  Opaques = erl_types:module_builtin_opaques(Module) ++
    erl_types:t_opaque_from_records(Records),
  TreeMap = build_tree_map(Tree),
  Funs = dict:fetch_keys(TreeMap),
  FunTab = init_fun_tab(Funs, dict:new(), TreeMap, Callgraph, Plt, Opaques),
  Work = init_work([get_label(Tree)]),
  Env = dict:store(top, map__new(), dict:new()),
  #state{callgraph = Callgraph, envs = Env, fun_tab = FunTab, opaques = Opaques,
	 plt = Plt, races = dialyzer_races:new(), records = Records,
	 warning_mode = false, warnings = [], work = Work, tree_map = TreeMap,
	 module = Module, behaviour_api_dict = BehaviourTranslations}.

state__mark_fun_as_handled(#state{fun_tab = FunTab} = State, Fun0) ->
  Fun = get_label(Fun0),
  case dict:find(Fun, FunTab) of
    {ok, {not_handled, Entry}} ->
      State#state{fun_tab = dict:store(Fun, Entry, FunTab)};
    {ok, {_, _}} ->
      State
  end.

state__warning_mode(#state{warning_mode = WM}) ->
  WM.

state__set_warning_mode(#state{tree_map = TreeMap, fun_tab = FunTab,
                               races = Races} = State) ->
  ?debug("Starting warning pass\n", []),
  Funs = dict:fetch_keys(TreeMap),
  State#state{work = init_work([top|Funs--[top]]),
	      fun_tab = FunTab, warning_mode = true,
              races = dialyzer_races:put_race_analysis(true, Races)}.

state__restore_race_code(RaceCode, #state{callgraph = Callgraph} = State) ->
  State#state{callgraph = dialyzer_callgraph:put_race_code(RaceCode,
                                                           Callgraph)}.

state__race_analysis(Analysis, #state{races = Races} = State) ->
  State#state{races = dialyzer_races:put_race_analysis(Analysis, Races)}.

state__renew_curr_fun(CurrFun, CurrFunLabel,
                      #state{races = Races} = State) ->
  State#state{races = dialyzer_races:put_curr_fun(CurrFun, CurrFunLabel,
                                                  Races)}.

state__renew_fun_args(Args, #state{races = Races} = State) ->
  case state__warning_mode(State) of
    true -> State;
    false ->
      State#state{races = dialyzer_races:put_fun_args(Args, Races)}
  end.

state__renew_race_list(RaceList, RaceListSize,
                       #state{races = Races} = State) ->
  State#state{races = dialyzer_races:put_race_list(RaceList, RaceListSize,
                                                   Races)}.

state__renew_warnings(Warnings, State) ->
  State#state{warnings = Warnings}.

-spec state__add_warning(dial_warning(), state()) -> state().

state__add_warning(Warn, #state{warnings = Warnings} = State) ->
  State#state{warnings = [Warn|Warnings]}.

state__add_warning(State, Tag, Tree, Msg) ->
  state__add_warning(State, Tag, Tree, Msg, false).

state__add_warning(#state{warning_mode = false} = State, _, _, _, _) ->
  State;
state__add_warning(#state{warnings = Warnings, warning_mode = true} = State,
		   Tag, Tree, Msg, Force) ->
  Ann = cerl:get_ann(Tree),
  case Force of
    true ->
      Warn = {Tag, {get_file(Ann), abs(get_line(Ann))}, Msg},
      State#state{warnings = [Warn|Warnings]};
    false ->
      case is_compiler_generated(Ann) of
	true -> State;
	false ->
	  Warn = {Tag, {get_file(Ann), get_line(Ann)}, Msg},
	  State#state{warnings = [Warn|Warnings]}
      end
  end.

state__get_race_warnings(#state{races = Races} = State) ->
  {Races1, State1} = dialyzer_races:get_race_warnings(Races, State),
  State1#state{races = Races1}.

state__get_warnings(#state{tree_map = TreeMap, fun_tab = FunTab,
			   callgraph = Callgraph, plt = Plt} = State,
		    NoWarnUnused) ->
  FoldFun =
    fun({top, _}, AccState) -> AccState;
       ({FunLbl, Fun}, AccState) ->
	{NotCalled, Ret} =
	  case dict:fetch(get_label(Fun), FunTab) of
	    {not_handled, {_Args0, Ret0}} -> {true, Ret0};
	    {Args0, Ret0} -> {any_none(Args0), Ret0}
	  end,
	case NotCalled of
	  true ->
	    {Warn, Msg} =
	      case dialyzer_callgraph:lookup_name(FunLbl, Callgraph) of
		error -> {true, {unused_fun, []}};
		{ok, {_M, F, A}} = MFA ->
		  {not sets:is_element(MFA, NoWarnUnused),
		   {unused_fun, [F, A]}}
	      end,
	    case Warn of
	      true -> state__add_warning(AccState, ?WARN_NOT_CALLED, Fun, Msg);
	      false -> AccState
	    end;
	  false ->
	    {Name, Contract} =
	      case dialyzer_callgraph:lookup_name(FunLbl, Callgraph) of
		error -> {[], none};
		{ok, {_M, F, A} = MFA} ->
		  {[F, A], dialyzer_plt:lookup_contract(Plt, MFA)}
	      end,
	    case t_is_none(Ret) of
	      true ->
		%% Check if the function has a contract that allows this.
		Warn =
		  case Contract of
		    none -> true;
		    {value, C} ->
		      GenRet = dialyzer_contracts:get_contract_return(C),
		      not t_is_unit(GenRet)
		  end,
		case Warn of
		  true ->
		    case classify_returns(Fun) of
		      no_match ->
			Msg = {no_return, [no_match|Name]},
			state__add_warning(AccState, ?WARN_RETURN_NO_RETURN,
					   Fun, Msg);
		      only_explicit ->
			Msg = {no_return, [only_explicit|Name]},
			state__add_warning(AccState, ?WARN_RETURN_ONLY_EXIT,
					   Fun, Msg);
		      only_normal ->
			Msg = {no_return, [only_normal|Name]},
			state__add_warning(AccState, ?WARN_RETURN_NO_RETURN,
					   Fun, Msg);
		      both ->
			Msg = {no_return, [both|Name]},
			state__add_warning(AccState, ?WARN_RETURN_NO_RETURN,
					   Fun, Msg)
		    end;
		  false ->
		    AccState
		end;
	      false ->
		AccState
	    end
	end
    end,
  #state{warnings = Warn} = lists:foldl(FoldFun, State, dict:to_list(TreeMap)),
  Warn.

state__is_escaping(Fun, #state{callgraph = Callgraph}) ->
  dialyzer_callgraph:is_escaping(Fun, Callgraph).

state__lookup_type_for_rec_var(Var, #state{callgraph = Callgraph} = State) ->
  Label = get_label(Var),
  case dialyzer_callgraph:lookup_rec_var(Label, Callgraph) of
    error -> error;
    {ok, MFA} ->
      {ok, FunLabel} = dialyzer_callgraph:lookup_label(MFA, Callgraph),
      {ok, state__fun_type(FunLabel, State)}
  end.

state__lookup_name({_, _, _} = MFA, #state{}) ->
  MFA;
state__lookup_name(top, #state{}) ->
  top;
state__lookup_name(Fun, #state{callgraph = Callgraph}) ->
  case dialyzer_callgraph:lookup_name(Fun, Callgraph) of
    {ok, MFA} -> MFA;
    error -> Fun
  end.

state__lookup_record(Tag, Arity, #state{records = Records}) ->
  case erl_types:lookup_record(Tag, Arity, Records) of
    {ok, Fields} ->
      {ok, t_tuple([t_atom(Tag)|
		    [FieldType || {_FieldName, FieldType} <- Fields]])};
    error ->
      error
  end.

state__get_args(Tree, #state{fun_tab = FunTab}) ->
  Fun = get_label(Tree),
  case dict:find(Fun, FunTab) of
    {ok, {not_handled, {ArgTypes, _}}} -> ArgTypes;
    {ok, {ArgTypes, _}} -> ArgTypes
  end.

build_tree_map(Tree) ->
  Fun =
    fun(T, Dict) ->
	case cerl:is_c_fun(T) of
	  true ->
	    dict:store(get_label(T), T, Dict);
	  false ->
	    Dict
	end
    end,
  cerl_trees:fold(Fun, dict:new(), Tree).

init_fun_tab([top|Left], Dict, TreeMap, Callgraph, Plt, Opaques) ->
  NewDict = dict:store(top, {not_handled, {[], t_none()}}, Dict),
  init_fun_tab(Left, NewDict, TreeMap, Callgraph, Plt, Opaques);
init_fun_tab([Fun|Left], Dict, TreeMap, Callgraph, Plt, Opaques) ->
  Arity = cerl:fun_arity(dict:fetch(Fun, TreeMap)),
  FunEntry =
    case dialyzer_callgraph:is_escaping(Fun, Callgraph) of
      true ->
        Args = lists:duplicate(Arity, t_any()),
	case lookup_fun_sig(Fun, Callgraph, Plt) of
	  none -> {Args, t_unit()};
	  {value, {RetType, _}} ->
	    case t_is_none(RetType) of
	      true -> {Args, t_none()};
	      false -> {Args, t_unit()}
	    end
	end;
      false -> {lists:duplicate(Arity, t_none()), t_unit()}
    end,
  NewDict = dict:store(Fun, {not_handled, FunEntry}, Dict),
  init_fun_tab(Left, NewDict, TreeMap, Callgraph, Plt, Opaques);
init_fun_tab([], Dict, _TreeMap, _Callgraph, _Plt, _Opaques) ->
  Dict.

state__update_fun_env(Tree, Map, #state{envs = Envs} = State) ->
  NewEnvs = dict:store(get_label(Tree), Map, Envs),
  State#state{envs = NewEnvs}.

state__fun_env(Tree, #state{envs = Envs}) ->
  Fun = get_label(Tree),
  case dict:find(Fun, Envs) of
    error -> none;
    {ok, Map} -> Map
  end.

state__clean_not_called(#state{fun_tab = FunTab} = State) ->
  NewFunTab =
    dict:map(fun(top, Entry) -> Entry;
		(_Fun, {not_handled, {Args, _}}) -> {Args, t_none()};
		(_Fun, Entry) -> Entry
	     end, FunTab),
  State#state{fun_tab = NewFunTab}.

state__all_fun_types(#state{fun_tab = FunTab}) ->
  Tab1 = dict:erase(top, FunTab),
  dict:map(fun(_Fun, {Args, Ret}) -> t_fun(Args, Ret)end, Tab1).

state__fun_type(Fun, #state{fun_tab = FunTab}) ->
  Label =
    if is_integer(Fun) -> Fun;
       true -> get_label(Fun)
    end,
  case dict:find(Label, FunTab) of
    {ok, {not_handled, {A, R}}} ->
      t_fun(A, R);
    {ok, {A, R}} ->
      t_fun(A, R)
  end.

state__update_fun_entry(Tree, ArgTypes, Out0,
			#state{fun_tab=FunTab, callgraph=CG, plt=Plt} = State)->
  Fun = get_label(Tree),
  Out1 =
    if Fun =:= top -> Out0;
       true ->
	case lookup_fun_sig(Fun, CG, Plt) of
	  {value, {SigRet, _}} -> t_inf(SigRet, Out0, opaque);
	  none -> Out0
	end
    end,
  Out = t_limit(Out1, ?TYPE_LIMIT),
  case dict:find(Fun, FunTab) of
    {ok, {ArgTypes, OldOut}} ->
      case t_is_equal(OldOut, Out) of
	true ->
	  ?debug("Fixpoint for ~w: ~s\n",
		 [state__lookup_name(Fun, State),
		  t_to_string(t_fun(ArgTypes, Out))]),
	  State;
	false ->
	  NewEntry = {ArgTypes, Out},
	  ?debug("New Entry for ~w: ~s\n",
		 [state__lookup_name(Fun, State),
		  t_to_string(t_fun(ArgTypes, Out))]),
	  NewFunTab = dict:store(Fun, NewEntry, FunTab),
	  State1 = State#state{fun_tab = NewFunTab},
	  state__add_work_from_fun(Tree, State1)
      end;
    {ok, {NewArgTypes, _OldOut}} ->
      %% Can only happen in self-recursive functions. Only update the out type.
      NewEntry = {NewArgTypes, Out},
      ?debug("New Entry for ~w: ~s\n",
	     [state__lookup_name(Fun, State),
	      t_to_string(t_fun(NewArgTypes, Out))]),
      NewFunTab = dict:store(Fun, NewEntry, FunTab),
      State1 = State#state{fun_tab = NewFunTab},
      state__add_work_from_fun(Tree, State1)
  end.

state__add_work_from_fun(_Tree, #state{warning_mode = true} = State) ->
  State;
state__add_work_from_fun(Tree, #state{callgraph = Callgraph,
				      tree_map = TreeMap} = State) ->
  case get_label(Tree) of
    top -> State;
    Label when is_integer(Label) ->
      case dialyzer_callgraph:in_neighbours(Label, Callgraph) of
	none -> State;
	MFAList ->
	  LabelList = [dialyzer_callgraph:lookup_label(MFA, Callgraph)
		       || MFA <- MFAList],
	  %% Must filter the result for results in this module.
	  FilteredList = [L || {ok, L} <- LabelList, dict:is_key(L, TreeMap)],
	  ?debug("~w: Will try to add:~w\n",
		 [state__lookup_name(get_label(Tree), State), MFAList]),
	  lists:foldl(fun(L, AccState) ->
			  state__add_work(L, AccState)
		      end, State, FilteredList)
      end
  end.

state__add_work(external, State) ->
  State;
state__add_work(top, State) ->
  State;
state__add_work(Fun, #state{work = Work} = State) ->
  NewWork = add_work(Fun, Work),
  State#state{work = NewWork}.

state__get_work(#state{work = Work, tree_map = TreeMap} = State) ->
  case get_work(Work) of
    none -> none;
    {Fun, NewWork} ->
      {dict:fetch(Fun, TreeMap), State#state{work = NewWork}}
  end.

state__lookup_call_site(Tree, #state{callgraph = Callgraph}) ->
  Label = get_label(Tree),
  dialyzer_callgraph:lookup_call_site(Label, Callgraph).

state__fun_info(external, #state{}) ->
  external;
state__fun_info({_, _, _} = MFA, #state{plt = PLT}) ->
  {MFA,
   dialyzer_plt:lookup(PLT, MFA),
   dialyzer_plt:lookup_contract(PLT, MFA),
   t_any()};
state__fun_info(Fun, #state{callgraph = CG, fun_tab = FunTab, plt = PLT}) ->
  {Sig, Contract} =
    case dialyzer_callgraph:lookup_name(Fun, CG) of
      error ->
	{dialyzer_plt:lookup(PLT, Fun), none};
      {ok, MFA} ->
	{dialyzer_plt:lookup(PLT, MFA), dialyzer_plt:lookup_contract(PLT, MFA)}
    end,
  LocalRet =
    case dict:fetch(Fun, FunTab) of
      {not_handled, {_Args, Ret}} -> Ret;
      {_Args, Ret} -> Ret
    end,
  {Fun, Sig, Contract, LocalRet}.

state__find_apply_return(Tree, #state{callgraph = Callgraph} = State) ->
  Apply = get_label(Tree),
  case dialyzer_callgraph:lookup_call_site(Apply, Callgraph) of
    error ->
      unknown;
    {ok, List} ->
      case lists:member(external, List) of
	true -> t_any();
	false ->
	  FunTypes = [state__fun_type(F, State) || F <- List],
	  Returns = [t_fun_range(F) || F <- FunTypes],
	  t_sup(Returns)
      end
  end.

forward_args(Fun, ArgTypes, #state{work = Work, fun_tab = FunTab} = State) ->
  {OldArgTypes, OldOut, Fixpoint} =
    case dict:find(Fun, FunTab) of
      {ok, {not_handled, {OldArgTypes0, OldOut0}}} ->
	{OldArgTypes0, OldOut0, false};
      {ok, {OldArgTypes0, OldOut0}} ->
	{OldArgTypes0, OldOut0,
	 t_is_subtype(t_product(ArgTypes), t_product(OldArgTypes0))}
    end,
  case Fixpoint of
    true -> State;
    false ->
      NewArgTypes = [t_sup(X, Y) || {X, Y} <- lists:zip(ArgTypes, OldArgTypes)],
      NewWork = add_work(Fun, Work),
      ?debug("~w: forwarding args ~s\n",
	     [state__lookup_name(Fun, State),
	      t_to_string(t_product(NewArgTypes))]),
      NewFunTab = dict:store(Fun, {NewArgTypes, OldOut}, FunTab),
      State#state{work = NewWork, fun_tab = NewFunTab}
  end.

-spec state__cleanup(state()) -> state().

state__cleanup(#state{callgraph = Callgraph,
                      races = Races,
                      records = Records}) ->
  #state{callgraph = dialyzer_callgraph:cleanup(Callgraph),
         races = dialyzer_races:cleanup(Races),
         records = Records}.

-spec state__get_callgraph(state()) -> dialyzer_callgraph:callgraph().

state__get_callgraph(#state{callgraph = Callgraph}) ->
  Callgraph.

-spec state__get_races(state()) -> dialyzer_races:races().

state__get_races(#state{races = Races}) ->
  Races.

-spec state__get_records(state()) -> dict().

state__get_records(#state{records = Records}) ->
  Records.

-spec state__put_callgraph(dialyzer_callgraph:callgraph(), state()) ->
  state().

state__put_callgraph(Callgraph, State) ->
  State#state{callgraph = Callgraph}.

-spec state__put_races(dialyzer_races:races(), state()) -> state().

state__put_races(Races, State) ->
  State#state{races = Races}.

-spec state__records_only(state()) -> state().

state__records_only(#state{records = Records}) ->
  #state{records = Records}.

%%% ===========================================================================
%%%
%%%  Races
%%%
%%% ===========================================================================

renew_code(Fun, FunArgs, Code, WarningMode, Callgraph) ->
  case WarningMode of
    true -> Callgraph;
    false ->
      RaceCode = dialyzer_callgraph:get_race_code(Callgraph),
      dialyzer_callgraph:put_race_code(
        dict:store(Fun, [FunArgs, Code], RaceCode), Callgraph)
  end.

renew_public_tables([Var], Table, WarningMode, Callgraph) ->
  case WarningMode of
    true -> Callgraph;
    false ->
      case Table of
        no_t -> Callgraph;
        _Other ->
          VarLabel = get_label(Var),
          PTables = dialyzer_callgraph:get_public_tables(Callgraph),
          dialyzer_callgraph:put_public_tables(
            lists:usort([VarLabel|PTables]), Callgraph)
      end
  end.

%%% ===========================================================================
%%%
%%%  Worklist
%%%
%%% ===========================================================================

init_work(List) ->
  {List, [], sets:from_list(List)}.

get_work({[], [], _Set}) ->
  none;
get_work({[H|T], Rev, Set}) ->
  {H, {T, Rev, sets:del_element(H, Set)}};
get_work({[], Rev, Set}) ->
  get_work({lists:reverse(Rev), [], Set}).

add_work(New, {List, Rev, Set} = Work) ->
  case sets:is_element(New, Set) of
    true -> Work;
    false -> {List, [New|Rev], sets:add_element(New, Set)}
  end.

%%% ===========================================================================
%%%
%%%  Utilities.
%%%
%%% ===========================================================================

get_line([Line|_]) when is_integer(Line) -> Line;
get_line([_|Tail]) -> get_line(Tail);
get_line([]) -> -1.

get_file([]) -> [];
get_file([{file, File}|_]) -> File;
get_file([_|Tail]) -> get_file(Tail).

is_compiler_generated(Ann) ->
  lists:member(compiler_generated, Ann) orelse (get_line(Ann) < 1).

-spec format_args([cerl:cerl()], [erl_types:erl_type()], state()) ->
  nonempty_string().

format_args([], [], _State) ->
  "()";
format_args(ArgList, TypeList, State) ->
  "(" ++ format_args_1(ArgList, TypeList, State) ++ ")".

format_args_1([Arg], [Type], State) ->
  format_arg(Arg) ++ format_type(Type, State);
format_args_1([Arg|Args], [Type|Types], State) ->
  String =
    case cerl:is_literal(Arg) of
      true -> format_cerl(Arg);
      false -> format_arg(Arg) ++ format_type(Type, State)
    end,
  String ++ "," ++ format_args_1(Args, Types, State).

format_arg(Arg) ->
  Default = "",
  case cerl:is_c_var(Arg) of
    true ->
      case cerl:var_name(Arg) of
	Atom when is_atom(Atom) ->
	  case atom_to_list(Atom) of
	    "cor"++_ -> Default;
	    "rec"++_ -> Default;
	    Name -> Name ++ "::"
	  end;
	_What -> Default
      end;
    false ->
      Default
  end.

-spec format_type(erl_types:erl_type(), state()) -> string().

format_type(Type, #state{records = R}) ->
  t_to_string(Type, R).

-spec format_field_diffs(erl_types:erl_type(), state()) -> string().

format_field_diffs(RecConstruction, #state{records = R}) ->
  erl_types:record_field_diffs_to_string(RecConstruction, R).

-spec format_sig_args(erl_types:erl_type(), state()) -> string().

format_sig_args(Type, #state{records = R}) ->
  SigArgs = t_fun_args(Type),
  case SigArgs of
    [] -> "()";
    [SArg|SArgs] ->
      lists:flatten("(" ++ t_to_string(SArg, R)
		        ++ ["," ++ t_to_string(T, R) || T <- SArgs] ++ ")")
    end.

format_cerl(Tree) ->
  cerl_prettypr:format(cerl:set_ann(Tree, []),
		       [{hook, dialyzer_utils:pp_hook()},
			{noann, true},
			{paper, 100000}, %% These guys strip
			{ribbon, 100000} %% newlines.
		       ]).

format_patterns(Pats) ->
  NewPats = map_pats(cerl:c_values(Pats)),
  String = format_cerl(NewPats),
  case Pats of
    [PosVar] ->
      case cerl:is_c_var(PosVar) andalso (cerl:var_name(PosVar) =/= '') of
	true -> "variable "++String;
	false -> "pattern "++String
      end;
    _ ->
      "pattern "++String
  end.

map_pats(Pats) ->
  Fun = fun(Tree) ->
	    case cerl:is_c_var(Tree) of
	      true ->
		case cerl:var_name(Tree) of
		  Atom when is_atom(Atom) ->
		    case atom_to_list(Atom) of
		      "cor"++_ -> cerl:c_var('');
		      "rec"++_ -> cerl:c_var('');
		      _ -> cerl:set_ann(Tree, [])
		    end;
		  _What -> cerl:c_var('')
		end;
	      false ->
		cerl:set_ann(Tree, [])
	    end
	end,
  cerl_trees:map(Fun, Pats).

classify_returns(Tree) ->
  case find_terminals(cerl:fun_body(Tree)) of
    {false, false} -> no_match;
    {true, false} -> only_explicit;
    {false, true} -> only_normal;
    {true, true} -> both
  end.

find_terminals(Tree) ->
  case cerl:type(Tree) of
    apply -> {false, true};
    binary -> {false, true};
    bitstr -> {false, true};
    call ->
      M0 = cerl:call_module(Tree),
      F0 = cerl:call_name(Tree),
      A = length(cerl:call_args(Tree)),
      case cerl:is_literal(M0) andalso cerl:is_literal(F0) of
	false ->
	  %% We cannot make assumptions. Say that both are true.
	  {true, true};
	true ->
	  M = cerl:concrete(M0),
	  F = cerl:concrete(F0),
	  case (erl_bif_types:is_known(M, F, A)
		andalso t_is_none(erl_bif_types:type(M, F, A))) of
	    true -> {true, false};
	    false -> {false, true}
	  end
      end;
    'case' -> find_terminals_list(cerl:case_clauses(Tree));
    'catch' -> find_terminals(cerl:catch_body(Tree));
    clause -> find_terminals(cerl:clause_body(Tree));
    cons -> {false, true};
    'fun' -> {false, true};
    'let' -> find_terminals(cerl:let_body(Tree));
    letrec -> find_terminals(cerl:letrec_body(Tree));
    literal -> {false, true};
    primop -> {false, false}; %% match_fail, etc. are not explicit exits.
    'receive' ->
      Timeout = cerl:receive_timeout(Tree),
      Clauses = cerl:receive_clauses(Tree),
      case (cerl:is_literal(Timeout) andalso
	    (cerl:concrete(Timeout) =:= infinity)) of
	true ->
	  if Clauses =:= [] -> {false, true}; %% A never ending receive.
	     true -> find_terminals_list(Clauses)
	  end;
	false -> find_terminals_list([cerl:receive_action(Tree)|Clauses])
      end;
    seq -> find_terminals(cerl:seq_body(Tree));
    'try' ->
      find_terminals_list([cerl:try_handler(Tree), cerl:try_body(Tree)]);
    tuple -> {false, true};
    values -> {false, true};
    var -> {false, true}
  end.

find_terminals_list(List) ->
  find_terminals_list(List, false, false).

find_terminals_list([Tree|Left], Explicit1, Normal1) ->
  {Explicit2, Normal2} = find_terminals(Tree),
  case {Explicit1 or Explicit2, Normal1 or Normal2} of
    {true, true} = Ans -> Ans;
    {NewExplicit, NewNormal} ->
      find_terminals_list(Left, NewExplicit, NewNormal)
  end;
find_terminals_list([], Explicit, Normal) ->
  {Explicit, Normal}.

%%----------------------------------------------------------------------------

%% If you write a record pattern in a matching that violates the
%% definition it will never match. However, the warning is lost in the
%% regular analysis. This after-pass catches it.

find_mismatched_record_patterns(Tree, State) ->
  cerl_trees:fold(
    fun(SubTree, AccState) ->
	case cerl:is_c_clause(SubTree) of
	  true -> lists:foldl(fun(P, AccState1) ->
				  find_rec_warnings(P, AccState1)
			      end, AccState, cerl:clause_pats(SubTree));
	  false -> AccState
	end
    end, State, Tree).

find_rec_warnings(Tree, State) ->
  cerl_trees:fold(
    fun(SubTree, AccState) ->
	case cerl:is_c_tuple(SubTree) of
	  true -> find_rec_warnings_tuple(SubTree, AccState);
	  false -> AccState
	end
    end, State, Tree).

find_rec_warnings_tuple(Tree, State) ->
  Elements = cerl:tuple_es(Tree),
  {_, _, EsType} = traverse_list(Elements, map__new(), State),
  TupleType = t_tuple(EsType),
  case t_is_none(TupleType) of
    true -> State;
    false ->
      %% Let's find out if this is a record construction.
      case Elements of
	[Tag|Left] ->
	  case cerl:is_c_atom(Tag) of
	    true ->
	      TagVal = cerl:atom_val(Tag),
	      case state__lookup_record(TagVal, length(Left), State) of
		error -> State;
		{ok, Prototype} ->
		  InfTupleType = t_inf(Prototype, TupleType),
		  case t_is_none(InfTupleType) of
		    true ->
		      Msg = {record_matching,
			     [format_patterns([Tree]), TagVal]},
		      state__add_warning(State, ?WARN_MATCHING, Tree, Msg);
		    false ->
		      State
		  end
	      end;
	    false ->
	      State
	  end;
	_ ->
	  State
      end
  end.

%%----------------------------------------------------------------------------

-ifdef(DEBUG_PP).
debug_pp(Tree, true) ->
  io:put_chars(cerl_prettypr:format(Tree, [{hook, cerl_typean:pp_hook()}])),
  io:nl(),
  ok;
debug_pp(Tree, false) ->
  io:put_chars(cerl_prettypr:format(strip_annotations(Tree))),
  io:nl(),
  ok.

strip_annotations(Tree) ->
  Fun = fun(T) ->
	    case cerl:type(T) of
	      var ->
		cerl:set_ann(T, [{label, cerl_trees:get_label(T)}]);
	      'fun' ->
		cerl:set_ann(T, [{label, cerl_trees:get_label(T)}]);
	      _ ->
		cerl:set_ann(T, [])
	    end
	end,
  cerl_trees:map(Fun, Tree).

-else.

debug_pp(_Tree, _UseHook) ->
  ok.
-endif.

%%----------------------------------------------------------------------------

-spec to_dot(dialyzer_callgraph:callgraph()) -> 'ok'.

-ifdef(DOT).
to_dot(CG) ->
  dialyzer_callgraph:to_dot(CG).
-else.
to_dot(_CG) ->
  ok.
-endif.

