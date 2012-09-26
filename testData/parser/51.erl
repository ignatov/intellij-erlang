%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.erlang.org/EPLICENSE
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Original Code is exprecs-0.2.
%%
%% Copyright (c) 2010 Erlang Solutions Ltd.
%% The Initial Developer of the Original Code is Ericsson AB.
%% Portions created by Ericsson are Copyright (C), 2006, Ericsson AB.
%% All Rights Reserved.
%%
%% Contributor(s): ______________________________________.

%%-------------------------------------------------------------------
%% File    : exprecs.erl
%% @author  : Ulf Wiger <ulf.wiger@ericsson.com>
%% @end
%% Description :
%%
%% Created : 13 Feb 2006 by Ulf Wiger <ulf.wiger@ericsson.com>
%% Rewritten: Jan-Feb 2010 by Ulf Wiger <ulf.wiger@elang-solutions.com>
%%-------------------------------------------------------------------

%% @doc Parse transform for generating record access functions.
%% <p>This parse transform can be used to reduce compile-time
%% dependencies in large systems.</p>
%% <p>In the old days, before records, Erlang programmers often wrote
%% access functions for tuple data. This was tedious and error-prone.
%% The record syntax made this easier, but since records were implemented
%% fully in the pre-processor, a nasty compile-time dependency was
%% introduced.</p>
%% <p>This module automates the generation of access functions for
%% records. While this method cannot fully replace the utility of
%% pattern matching, it does allow a fair bit of functionality on
%% records without the need for compile-time dependencies.</p>
%% <p>Whenever record definitions need to be exported from a module,
%% inserting a compiler attribute,
%% <code>export_records([RecName|...])</code> causes this transform
%% to lay out access functions for the exported records:</p>
%%
%% As an example, consider the following module:
%% <pre>
%% -module(test_exprecs).
%% -export([f/0]).
%%
%% -compile({parse_transform, exprecs}).
%%
%% -record(r, {a = 0 :: integer(),
%%             b = 0 :: integer(),
%%             c = 0 :: integer()}).
%%
%% -record(s,{a}).
%%
%% -export_records([r,s]).
%%
%% f() -&gt;
%%     {new,'#new-r'([])}.
%% </pre>
%%
%% <pre>
%% -module(test_exprecs).
%% -compile({pt_pp_src,true}).
%% -export([f/0]).
%% -record(r,{a = 0 :: integer(),b = 0 :: integer(),c = 0 :: integer()}).
%% -record(s,{a}).
%% -export_records([r,s]).
%% -export(['#exported_records-'/0,
%%          '#new-'/1,
%%          '#info-'/1,
%%          '#info-'/2,
%%          '#pos-'/2,
%%          '#is_record-'/1,
%%          '#is_record-'/2,
%%          '#get-'/2,
%%          '#set-'/2,
%%          '#fromlist-'/2,
%%          '#new-r'/0,
%%          '#new-r'/1,
%%          '#get-r'/2,
%%          '#set-r'/2,
%%          '#pos-r'/1,
%%          '#fromlist-r'/1,
%%          '#fromlist-r'/2,
%%          '#info-r'/1,
%%          '#new-s'/0,
%%          '#new-s'/1,
%%          '#get-s'/2,
%%          '#set-s'/2,
%%          '#pos-s'/1,
%%          '#fromlist-s'/1,
%%          '#fromlist-s'/2,
%%          '#info-s'/1]).
%%
%% -type '#prop-r'() :: {a, integer()} | {b, integer()} | {c, integer()}.
%% -type '#attr-r'() :: a | b | c.
%% -type '#prop-s'() :: {a, any()}.
%% -type '#attr-s'() :: a.
%%
%% -spec '#exported_records-'() -&gt; [r | s].
%% '#exported_records-'() -&gt;
%%     [r,s].
%%
%% -spec '#new-'(r) -&gt; #r{};
%%              (s) -&gt; #s{}.
%% '#new-'(r) -&gt;
%%     '#new-r'();
%% '#new-'(s) -&gt;
%%     '#new-s'().
%%
%% -spec '#info-'(r) -&gt; [a | b | c];
%%               (s) -&gt; [a].
%% '#info-'(RecName) -&gt;
%%     '#info-'(RecName, fields).
%%
%% -spec '#info-'(r, size) -&gt; 4;
%%               (r, fields) -&gt; [a | b | c];
%%               (s, size) -&gt; 2;
%%               (s, fields) -&gt; [a].
%% '#info-'(r, Info) -&gt;
%%     '#info-r'(Info);
%% '#info-'(s, Info) -&gt;
%%     '#info-s'(Info).
%%
%% -spec '#pos-'(r, a) -&gt; 1;
%%              (r, b) -&gt; 2;
%%              (r, c) -&gt; 3;
%%              (s, a) -&gt; 1.
%% '#pos-'(r, Attr) -&gt;
%%     '#pos-r'(Attr);
%% '#pos-'(s, Attr) -&gt;
%%     '#pos-s'(Attr).
%%
%% -spec '#is_record-'(#r{}) -&gt; true;
%%                    (#s{}) -&gt; true;
%%                    (any()) -&gt; false.
%% '#is_record-'(X) -&gt;
%%     if
%%         is_record(X, r) -&gt;
%%             true;
%%         is_record(X, s) -&gt;
%%             true;
%%         true -&gt;
%%             false
%%     end.
%%
%% -spec '#is_record-'(r, #r{}) -&gt; true;
%%                    (s, #s{}) -&gt; true;
%%                    (any(), any()) -&gt; false.
%% '#is_record-'(s, Rec) when tuple_size(Rec) == 2, element(1, Rec) == s -&gt;
%%     true;
%% '#is_record-'(r, Rec) when tuple_size(Rec) == 4, element(1, Rec) == r -&gt;
%%     true;
%% '#is_record-'(_, _) -&gt;
%%     false.
%%
%% -spec '#get-'(a, #r{}) -&gt; integer();
%%              (b, #r{}) -&gt; integer();
%%              (c, #r{}) -&gt; integer();
%%              (a, #s{}) -&gt; any();
%%              (['#attr-r'()], #r{}) -&gt; [integer()];
%%              (['#attr-s'()], #s{}) -&gt; [any()].
%% '#get-'(Attrs, Rec) when is_record(Rec, r) -&gt;
%%     '#get-r'(Attrs, Rec);
%% '#get-'(Attrs, Rec) when is_record(Rec, s) -&gt;
%%     '#get-s'(Attrs, Rec).
%%
%% -spec '#set-'(['#prop-r'()], #r{}) -&gt; #r{};
%%              (['#prop-s'()], #s{}) -&gt; #s{}.
%% '#set-'(Vals, Rec) when is_record(Rec, r) -&gt;
%%     '#set-r'(Vals, Rec);
%% '#set-'(Vals, Rec) when is_record(Rec, s) -&gt;
%%     '#set-s'(Vals, Rec).
%%
%% -spec '#fromlist-'(['#prop-r'()], #r{}) -&gt; #r{};
%%                   (['#prop-s'()], #s{}) -&gt; #s{}.
%% '#fromlist-'(Vals, Rec) when is_record(Rec, r) -&gt;
%%     '#fromlist-r'(Vals, Rec);
%% '#fromlist-'(Vals, Rec) when is_record(Rec, s) -&gt;
%%     '#fromlist-s'(Vals, Rec).
%%
%% -spec '#new-r'() -&gt; #r{}.
%% '#new-r'() -&gt;
%%     #r{}.
%%
%% -spec '#new-r'(['#prop-r'()]) -&gt; #r{}.
%% '#new-r'(Vals) -&gt;
%%     '#set-r'(Vals, #r{}).
%%
%% -spec '#get-r'(a, #r{}) -&gt; integer();
%%               (b, #r{}) -&gt; integer();
%%               (c, #r{}) -&gt; integer();
%%               (['#attr-r'()], #r{}) -&gt; [integer()].
%% '#get-r'(Attrs, R) when is_list(Attrs) -&gt;
%%     [
%%      '#get-r'(A, R) ||
%%          A &lt;- Attrs
%%     ];
%% '#get-r'(a, R) -&gt;
%%     R#r.a;
%% '#get-r'(b, R) -&gt;
%%     R#r.b;
%% '#get-r'(c, R) -&gt;
%%     R#r.c;
%% '#get-r'(Attr, R) -&gt;
%%     error(bad_record_op, ['#get-r',Attr,R]).
%%
%% -spec '#set-r'(['#prop-r'()], #r{}) -&gt; #r{}.
%% '#set-r'(Vals, Rec) -&gt;
%%     F = fun([], R, _F1) -&gt;
%%                R;
%%            ([{a,V}|T], R, F1) when is_list(T) -&gt;
%%                F1(T, R#r{a = V}, F1);
%%            ([{b,V}|T], R, F1) when is_list(T) -&gt;
%%                F1(T, R#r{b = V}, F1);
%%            ([{c,V}|T], R, F1) when is_list(T) -&gt;
%%                F1(T, R#r{c = V}, F1);
%%            (Vs, R, _) -&gt;
%%                error(bad_record_op, ['#set-r',Vs,R])
%%         end,
%%     F(Vals, Rec, F).
%%
%% -spec '#fromlist-r'(['#prop-r'()]) -&gt; #r{}.
%% '#fromlist-r'(Vals) when is_list(Vals) -&gt;
%%     '#fromlist-r'(Vals, '#new-r'()).
%%
%% -spec '#fromlist-r'(['#prop-r'()], #r{}) -&gt; #r{}.
%% '#fromlist-r'(Vals, Rec) -&gt;
%%     AttrNames = [{a,2},{b,3},{c,4}],
%%     F = fun([], R, _F1) -&gt;
%%                R;
%%            ([{H,Pos}|T], R, F1) when is_list(T) -&gt;
%%                case lists:keyfind(H, 1, Vals) of
%%                    false -&gt;
%%                        F1(T, R, F1);
%%                    {_,Val} -&gt;
%%                        F1(T, setelement(Pos, R, Val), F1)
%%                end
%%         end,
%%     F(AttrNames, Rec, F).
%%
%% -spec '#pos-r'('#attr-r'() | atom()) -&gt; integer().
%% '#pos-r'(a) -&gt;
%%     2;
%% '#pos-r'(b) -&gt;
%%     3;
%% '#pos-r'(c) -&gt;
%%     4;
%% '#pos-r'(A) when is_atom(A) -&gt;
%%     0.
%%
%% -spec '#info-r'(fields) -&gt; [a | b | c];
%%                (size) -&gt; 3.
%% '#info-r'(fields) -&gt;
%%     record_info(fields, r);
%% '#info-r'(size) -&gt;
%%     record_info(size, r).
%%
%% -spec '#new-s'() -&gt; #s{}.
%% '#new-s'() -&gt;
%%     #s{}.
%%
%% -spec '#new-s'(['#prop-s'()]) -&gt; #s{}.
%% '#new-s'(Vals) -&gt;
%%     '#set-s'(Vals, #s{}).
%%
%% -spec '#get-s'(a, #s{}) -&gt; any();
%%               (['#attr-s'()], #s{}) -&gt; [any()].
%% '#get-s'(Attrs, R) when is_list(Attrs) -&gt;
%%     [
%%      '#get-s'(A, R) ||
%%          A &lt;- Attrs
%%     ];
%% '#get-s'(a, R) -&gt;
%%     R#s.a;
%% '#get-s'(Attr, R) -&gt;
%%     error(bad_record_op, ['#get-s',Attr,R]).
%%
%% -spec '#set-s'(['#prop-s'()], #s{}) -&gt; #s{}.
%% '#set-s'(Vals, Rec) -&gt;
%%     F = fun([], R, _F1) -&gt;
%%                R;
%%            ([{a,V}|T], R, F1) when is_list(T) -&gt;
%%                F1(T, R#s{a = V}, F1);
%%            (Vs, R, _) -&gt;
%%                error(bad_record_op, ['#set-s',Vs,R])
%%         end,
%%     F(Vals, Rec, F).
%%
%% -spec '#fromlist-s'(['#prop-s'()]) -&gt; #s{}.
%% '#fromlist-s'(Vals) when is_list(Vals) -&gt;
%%     '#fromlist-s'(Vals, '#new-s'()).
%%
%% -spec '#fromlist-s'(['#prop-s'()], #s{}) -&gt; #s{}.
%% '#fromlist-s'(Vals, Rec) -&gt;
%%     AttrNames = [{a,2}],
%%     F = fun([], R, _F1) -&gt;
%%                R;
%%            ([{H,Pos}|T], R, F1) when is_list(T) -&gt;
%%                case lists:keyfind(H, 1, Vals) of
%%                    false -&gt;
%%                        F1(T, R, F1);
%%                    {_,Val} -&gt;
%%                        F1(T, setelement(Pos, R, Val), F1)
%%                end
%%         end,
%%     F(AttrNames, Rec, F).
%%
%% -spec '#pos-s'('#attr-s'() | atom()) -&gt; integer().
%% '#pos-s'(a) -&gt;
%%     2;
%% '#pos-s'(A) when is_atom(A) -&gt;
%%     0.
%%
%% -spec '#info-s'(fields) -&gt; [a];
%%                (size) -&gt; 1.
%% '#info-s'(fields) -&gt;
%%     record_info(fields, s);
%% '#info-s'(size) -&gt;
%%     record_info(size, s).
%%
%% f() -&gt;
%%     {new,'#new-r'([])}.
%%
%% </pre>
%%
%% It is possible to modify the naming rules of exprecs, through the use
%% of the following attributes (example reflecting the current rules):
%%
%% <pre>
%% -exprecs_prefix(["#", operation, "-"]).
%% -exprecs_fname([prefix, record]).
%% -exprecs_vfname([fname, "__", version]).
%% </pre>
%%
%% The lists must contain strings or any of the following control atoms:
%% <ul>
%% <li>in `exprecs_prefix': `operation'</li>
%% <li>in `exprecs_fname': `operation', `record', `prefix'</li>
%% <li>in `exprecs_vfname': `operation', `record', `prefix', `fname', `version'
%% </li>
%% </ul>
%%
%% Exprecs will substitute the control atoms with the string values of the
%% corresponding items. The result will then be flattened and converted to an
%% atom (a valid function or type name).
%%
%% `operation' is one of:
%% <ul>
%% <li>`new'</li>
%% <li>`get'</li>
%% <li>`set'</li>
%% <li>`fromlist'</li>
%% <li>`info'</li>
%% <li>`pos'</li>
%% <li>`is_record'</li>
%% <li>`convert'</li>
%% <li>`prop'</li>
%% <li>`attr'</li>
%% </ul>
%%
%% @end

-module(exprecs).

-export([parse_transform/2,
	 format_error/1,
%	 transform/3,
	 context/2]).

-record(context, {module,
		  function,
		  arity}).

-record(pass1, {exports = [],
		generated = false,
		records = [],
		record_types = [],
                versions = orddict:new(),
                inserted = false,
		prefix = ["#", operation, "-"],
		fname = [prefix, record],
		vfname = [fname, "__", version]}).

-include("../include/codegen.hrl").

-define(HERE, {?MODULE, ?LINE}).

-define(ERROR(R, F, I),
        begin
            rpt_error(R, F, I),
            throw({error,get_pos(I),{unknown,R}})
        end).

-type form()    :: any().
-type forms()   :: [form()].
-type options() :: [{atom(), any()}].


get_pos(I) ->
    case proplists:get_value(form, I) of
	undefined ->
	    0;
	Form ->
	    erl_syntax:get_pos(Form)
    end.

-spec parse_transform(forms(), options()) ->
    forms().
parse_transform(Forms, Options) ->
    parse_trans:top(fun do_transform/2, Forms, Options).

do_transform(Forms, Context) ->
    Acc1 = versioned_records(
	     add_untyped_recs(
	       parse_trans:do_inspect(fun inspect_f/4, #pass1{},
				      Forms, Context))),
    {Forms2, Acc2} =
	parse_trans:do_transform(fun generate_f/4, Acc1, Forms, Context),
    parse_trans:revert(verify_generated(Forms2, Acc2, Context)).

add_untyped_recs(#pass1{records = Rs,
			record_types = RTypes,
			exports = Es} = Acc) ->
    Untyped =
	[{R, Def} || {R, Def} <- Rs,
		     lists:member(R, Es),
		     not lists:keymember(R, 1, RTypes)],
    RTypes1 = [{R, lists:map(
		     fun({record_field,L,{atom,_,A}}) -> {A, t_any(L)};
			({record_field,L,{atom,_,A},_}) -> {A, t_any(L)}
		     end, Def)} || {R, Def} <- Untyped],
    Acc#pass1{record_types = RTypes ++ RTypes1}.

inspect_f(attribute, {attribute,_L,exprecs_prefix,Pattern}, _Ctxt, Acc) ->
    {false, Acc#pass1{prefix = Pattern}};
inspect_f(attribute, {attribute,_L,exprecs_fname,Pattern}, _Ctxt, Acc) ->
    {false, Acc#pass1{fname = Pattern}};
inspect_f(attribute, {attribute,_L,exprecs_vfname,Pattern}, _Ctxt, Acc) ->
    {false, Acc#pass1{vfname = Pattern}};
inspect_f(attribute, {attribute,_L,record,RecDef}, _Ctxt, Acc) ->
    Recs0 = Acc#pass1.records,
    {false, Acc#pass1{records = [RecDef|Recs0]}};
inspect_f(attribute, {attribute,_L,export_records, E}, _Ctxt, Acc) ->
    Exports0 = Acc#pass1.exports,
    NewExports = Exports0 ++ E,
    {false, Acc#pass1{exports = NewExports}};
inspect_f(attribute, {attribute, _L, type,
		      {{record, R}, RType,_}}, _Ctxt, Acc) ->
    Type = lists:map(
	     fun({typed_record_field, {record_field,_,{atom,_,A}}, T}) ->
		     {A, T};
		({typed_record_field, {record_field,_,{atom,_,A},_}, T}) ->
		     {A, T};
		({record_field, _, {atom,L,A}, _}) ->
		     {A, t_any(L)};
		({record_field, _, {atom,L,A}}) ->
		     {A, t_any(L)}
	     end, RType),
    {false, Acc#pass1{record_types = [{R, Type}|Acc#pass1.record_types]}};
inspect_f(_Type, _Form, _Context, Acc) ->
    {false, Acc}.

generate_f(attribute, {attribute,L,export_records,_} = Form, _Ctxt,
	    #pass1{exports = [_|_] = Es, versions = Vsns,
                   inserted = false} = Acc) ->
    case check_record_names(Es, L, Acc) of
	ok -> continue;
	{error, Bad} ->
	    ?ERROR(invalid_record_exports, ?HERE, Bad)
    end,
    Exports = [{fname(exported_records, Acc), 0},
	       {fname(new, Acc), 1},
	       {fname(info, Acc), 1},
	       {fname(info, Acc), 2},
               {fname(pos, Acc), 2},
	       {fname(is_record, Acc), 1},
	       {fname(is_record, Acc), 2},
	       {fname(get, Acc), 2},
	       {fname(set, Acc), 2},
	       {fname(fromlist, Acc), 2} |
	       lists:flatmap(
		 fun(Rec) ->
			 RecS = atom_to_list(Rec),
			 FNew = fname(new, RecS, Acc),
			 [{FNew, 0}, {FNew,1},
			  {fname(get, RecS, Acc), 2},
			  {fname(set, RecS, Acc), 2},
			  {fname(pos, RecS, Acc), 1},
			  {fname(fromlist, RecS, Acc), 1},
			  {fname(fromlist, RecS, Acc), 2},
			  {fname(info, RecS, Acc), 1}]
		 end, Es)] ++ version_exports(Vsns, Acc),
    {[], Form,
     [{attribute,L,export,Exports}],
     false, Acc#pass1{inserted = true}};
generate_f(function, Form, _Context, #pass1{generated = false} = Acc) ->
    % Layout record funs before first function
    L = erl_syntax:get_pos(Form),
    Forms = generate_specs_and_accessors(L, Acc),
    {Forms, Form, [], false, Acc#pass1{generated = true}};
generate_f(_Type, Form, _Ctxt, Acc) ->
    {Form, false, Acc}.

generate_specs_and_accessors(L, #pass1{exports = [_|_] = Es,
				       record_types = Ts} = Acc) ->
    Specs = generate_specs(L, [{R,T} || {R,T} <- Ts, lists:member(R, Es)], Acc),
    Funs = generate_accessors(L, Acc),
    Specs ++ Funs;
generate_specs_and_accessors(_, _) ->
    [].

verify_generated(Forms, #pass1{} = Acc, _Context) ->
    case (Acc#pass1.generated == true) orelse (Acc#pass1.exports == []) of
	true ->
	    Forms;
	false ->
	    % should be re-written to use the parse_trans helper...?
	    [{eof,Last}|RevForms] = lists:reverse(Forms),
	    [{function, NewLast, _, _, _}|_] = RevAs =
		lists:reverse(generate_specs_and_accessors(Last, Acc)),
	    lists:reverse([{eof, NewLast+1} | RevAs] ++ RevForms)
    end.


check_record_names(Es, L, #pass1{records = Rs}) ->
    case [E || E <- Es,
               not(lists:keymember(E, 1, Rs))] of
        [] ->
            ok;
        Bad ->
            {error, [{L,E} || E <- Bad]}
    end.

versioned_records(#pass1{exports = Es, records = Rs} = Pass1) ->
    case split_recnames(Rs) of
        [] ->
            Pass1#pass1{versions = []};
        [_|_] = Versions ->
            Exp_vsns =
                lists:foldl(
                  fun(Re, Acc) ->
                          case orddict:find(atom_to_list(Re), Versions) of
                              {ok, Vs} ->
                                  orddict:store(Re, Vs, Acc);
                              error ->
                                  Acc
                          end
                  end, orddict:new(), Es),
            Pass1#pass1{versions = Exp_vsns}
    end.

version_exports([], _Acc) ->
    [];
version_exports([_|_] = _Vsns, Acc) ->
    [{list_to_atom(fname_prefix(info, Acc)), 3},
     {list_to_atom(fname_prefix(convert, Acc)), 2}].


version_accessors(_L, #pass1{versions = []}) ->
    [];
version_accessors(L, #pass1{versions = Vsns} = Acc) ->
    Flat_vsns = flat_versions(Vsns),
    [f_convert(Vsns, L, Acc),
     f_info_3(Vsns, L, Acc)]
        ++ [f_info_1(Rname, Acc, L, V) || {Rname,V} <- Flat_vsns].

flat_versions(Vsns) ->
    lists:flatmap(fun({R,Vs}) ->
                          [{R,V} || V <- Vs]
                  end, Vsns).

split_recnames(Rs) ->
    lists:foldl(
      fun({R,_As}, Acc) ->
	      case re:split(atom_to_list(R), "__", [{return, list}]) of
                  [Base, V] ->
                      orddict:append(Base,V,Acc);
                  [_] ->
                      Acc
              end
      end, orddict:new(), Rs).

generate_specs(L, Specs, Acc) ->
    [[
      {attribute, L, type,
      {fname(prop, R, Acc),
       {type, L, union,
	[{type, L, tuple, [{atom,L,A},T]} || {A,T} <- Attrs]}, []}},
      {attribute, L, type,
       {fname(attr, R, Acc),
	{type, L, union,
	 [{atom, L, A} || {A,_} <- Attrs]}, []}}
     ] || {R, Attrs} <- Specs].


generate_accessors(L, Acc) ->
    lists:flatten(
      [f_exported_recs(Acc, L),
       f_new_(Acc, L),
       f_info(Acc, L),
       f_info_2(Acc, L),
       f_pos_2(Acc, L),
       f_isrec_1(Acc, L),
       f_isrec_2(Acc, L),
       f_get(Acc, L),
       f_set(Acc, L),
       f_fromlist(Acc, L) |
       lists:append(
	 lists:map(
	   fun(Rname) ->
		   Fields = get_flds(Rname, Acc),
		   [f_new_0(Rname, L, Acc),
		    f_new_1(Rname, L, Acc),
		    f_get_2(Rname, Fields, L, Acc),
		    f_set_2(Rname, Fields, L, Acc),
		    f_fromlist_1(Rname, L, Acc),
		    f_fromlist_2(Rname, Fields, L, Acc),
		    f_pos_1(Rname, Fields, L, Acc),
		    f_info_1(Rname, Acc, L)]
	   end, Acc#pass1.exports))] ++ version_accessors(L, Acc)).

get_flds(Rname, #pass1{records = Rs}) ->
    {_, Flds} = lists:keyfind(Rname, 1, Rs),
    lists:map(
      fun({record_field,_, {atom,_,N}}) -> N;
	 ({record_field,_, {atom,_,N}, _}) -> N
      end, Flds).


fname_prefix(Op, #pass1{prefix = Pat}) ->
    lists:flatten(
      lists:map(fun(operation) -> str(Op);
		   (X) -> str(X)
		end, Pat)).
%% fname_prefix(Op, #pass1{} = Acc) ->
%%     case Op of
%% 	new -> "#new-";
%% 	get -> "#get-";
%% 	set -> "#set-";
%% 	fromlist -> "#fromlist-";
%% 	info     -> "#info-";
%%         pos      -> "#pos-";
%% 	is_record   -> "#is_record-";
%%         convert  -> "#convert-";
%% 	prop     -> "#prop-";
%% 	attr     -> "#attr-"
%%     end.

%% fname_prefix(Op, Rname, Acc) ->
%%     fname_prefix(Op, Acc) ++ str(Rname).

str(A) when is_atom(A) ->
    atom_to_list(A);
str(S) when is_list(S) ->
    S.

fname(Op, #pass1{} = Acc) ->
    list_to_atom(fname_prefix(Op, Acc)).
    %% list_to_atom(fname_prefix(Op, Acc)).

fname(Op, Rname, #pass1{fname = FPat} = Acc) ->
    Prefix = fname_prefix(Op, Acc),
    list_to_atom(
      lists:flatten(
	lists:map(fun(prefix) -> str(Prefix);
		     (record) -> str(Rname);
		     (operation) -> str(Op);
		     (X) -> str(X)
		  end, FPat))).
    %% list_to_atom(fname_prefix(Op, Rname, Acc)).

fname(Op, Rname, V, #pass1{vfname = VPat} = Acc) ->
    list_to_atom(
      lists:flatten(
	lists:map(fun(prefix) -> fname_prefix(Op, Acc);
		     (operation) -> str(Op);
		     (record) -> str(Rname);
		     (version) -> str(V);
		     (fname) -> str(fname(Op, Rname, Acc));
		     (X) -> str(X)
		  end, VPat))).
    %% list_to_atom(fname_prefix(Op, Rname, Acc) ++ "__" ++ V).


%%% Meta functions

f_exported_recs(#pass1{exports = Es} = Acc, L) ->
    Fname = fname(exported_records, Acc),
    [funspec(L, Fname, [],
	     t_list(L, [t_union(L, [t_atom(L, E) || E <- Es])])),
     {function, L, Fname, 0,
      [{clause, L, [], [],
	[erl_parse:abstract(Es, L)]}]}
    ].

%%% Accessor functions
%%%
f_new_(#pass1{exports = Es} = Acc, L) ->
    Fname = fname(new, Acc),
    [funspec(L, Fname, [ {[t_atom(L, E)], t_record(L, E)} ||
			   E <- Es ]),
     {function, L, fname(new, Acc), 1,
      [{clause, L, [{atom, L, Re}], [],
	[{call, L, {atom, L, fname(new, Re, Acc)}, []}]}
       || Re <- Es]}
    ].

f_new_0(Rname, L, Acc) ->
    Fname = fname(new, Rname, Acc),
    [funspec(L, Fname, [], t_record(L, Rname)),
     {function, L, fname(new, Rname, Acc), 0,
      [{clause, L, [], [],
	[{record, L, Rname, []}]}]}
    ].


f_new_1(Rname, L, Acc) ->
    Fname = fname(new, Rname, Acc),
    [funspec(L, Fname, [t_list(L, [t_prop(L, Rname, Acc)])],
	     t_record(L, Rname)),
    {function, L, Fname, 1,
     [{clause, L, [{var, L, 'Vals'}], [],
       [{call, L, {atom, L, fname(set, Rname, Acc)},
	 [{var, L, 'Vals'},
	  {record, L, Rname, []}
	 ]}]
       }]}].

funspec(L, Fname, [{H,_} | _] = Alts) ->
    Arity = length(H),
    {attribute, L, spec,
     {{Fname, Arity},
      [{type, L, 'fun', [{type, L, product, Head}, Ret]} ||
	  {Head, Ret} <- Alts]}}.

funspec(L, Fname, Head, Returns) ->
    Arity = length(Head),
    {attribute, L, spec,
     {{Fname, Arity},
      [{type, L, 'fun',
	[{type, L, product, Head}, Returns]}]}}.


t_prop(L, Rname, Acc) -> {type, L, fname(prop, Rname, Acc), []}.
t_attr(L, Rname, Acc) -> {type, L, fname(attr, Rname, Acc), []}.
t_union(L, Alt)  -> {type, L, union, lists:usort(Alt)}.
t_any(L)         -> {type, L, any, []}.
t_atom(L)        -> {type, L, atom, []}.
t_atom(L, A)     -> {atom, L, A}.
t_integer(L)     -> {type, L, integer, []}.
t_integer(L, I)  -> {integer, L, I}.
t_list(L, Es)    -> {type, L, list, Es}.
%% t_tuple(L, Es) -> {type, L, tuple, Es}.
t_record(L, A)   -> {type, L, record, [{atom, L, A}]}.

f_set_2(Rname, Flds, L, Acc) ->
    Fname = fname(set, Rname, Acc),
    TRec = t_record(L, Rname),
    [funspec(L, Fname, [t_list(L, [t_prop(L, Rname, Acc)]), TRec],
	     TRec),
     {function, L, Fname, 2,
      [{clause, L, [{var, L, 'Vals'}, {var, L, 'Rec'}], [],
	[{match, L, {var, L, 'F'},
	  {'fun', L,
	   {clauses,
	    [{clause, L, [{nil,L},
			  {var,L,'R'},
			  {var,L,'_F1'}],
	      [],
	      [{var, L, 'R'}]} |
	     [{clause, L,
	       [{cons, L, {tuple, L, [{atom, L, Attr},
				      {var,  L, 'V'}]},
		 {var, L, 'T'}},
		{var, L, 'R'},
		{var, L, 'F1'}],
	       [[{call, L, {atom, L, is_list}, [{var, L, 'T'}]}]],
	       [{call, L, {var, L, 'F1'},
		 [{var,L,'T'},
		  {record, L, {var,L,'R'}, Rname,
		   [{record_field, L,
		     {atom, L, Attr},
		     {var, L, 'V'}}]},
		  {var, L, 'F1'}]}]} || Attr <- Flds]
	     ++ [{clause, L, [{var, L, 'Vs'}, {var,L,'R'},{var,L,'_'}],
		  [],
		  [bad_record_op(L, Fname, 'Vs', 'R')]}]
	    ]}}},
	 {call, L, {var, L, 'F'}, [{var, L, 'Vals'},
				   {var, L, 'Rec'},
				   {var, L, 'F'}]}]}]}].

bad_record_op(L, Fname, Val, R) ->
    {call, L, {remote, L, {atom,L,erlang}, {atom,L,error}},
     [{atom,L,bad_record_op}, {cons, L, {atom, L, Fname},
			       {cons, L, {var, L, Val},
				{cons, L, {var, L, R},
				 {nil, L}}}}]}.


f_pos_1(Rname, Flds, L, Acc) ->
    Fname = fname(pos, Rname, Acc),
    FieldList = lists:zip(Flds, lists:seq(2, length(Flds)+1)),
    [
     funspec(L, Fname, [t_union(L, [t_attr(L, Rname, Acc),
				    t_atom(L)])],
	     t_integer(L)),
     {function, L, Fname, 1,
      [{clause, L,
	[{atom, L, FldName}],
	[],
	[{integer, L, Pos}]} || {FldName, Pos} <- FieldList] ++
	  [{clause, L,
	    [{var, L, 'A'}],
	    [[{call, L, {atom, L, is_atom}, [{var, L, 'A'}]}]],
	    [{integer, L, 0}]}]
     }].

f_fromlist_1(Rname, L, Acc) ->
    Fname = fname(fromlist, Rname, Acc),
    [
     funspec(L, Fname, [t_list(L, [t_prop(L, Rname, Acc)])],
	     t_record(L, Rname)),
     {function, L, Fname, 1,
      [{clause, L, [{var, L, 'Vals'}],
	[[ {call, L, {atom, L, is_list}, [{var, L, 'Vals'}]} ]],
	[{call, L, {atom, L, Fname},
	  [{var, L, 'Vals'},
	   {call, L, {atom, L, fname(new, Rname, Acc)}, []}]}
	]}
      ]}].

f_fromlist_2(Rname, Flds, L, Acc) ->
    Fname = fname(fromlist, Rname, Acc),
    FldList = field_list(Flds),
    TRec = t_record(L, Rname),
    [
     funspec(L, Fname, [t_list(L, [t_prop(L, Rname, Acc)]), TRec],
	     TRec),
     {function, L, Fname, 2,
      [{clause, L, [{var, L, 'Vals'}, {var, L, 'Rec'}], [],
	[{match, L, {var, L, 'AttrNames'}, FldList},
	 {match, L, {var, L, 'F'},
	  {'fun', L,
	   {clauses,
	    [{clause, L, [{nil, L},
			  {var, L,'R'},
			  {var, L,'_F1'}],
	      [],
	      [{var, L, 'R'}]},
	     {clause, L, [{cons, L,
			   {tuple, L, [{var, L, 'H'},
				       {var, L, 'Pos'}]},
			   {var, L, 'T'}},
			  {var, L, 'R'}, {var, L, 'F1'}],
	      [[{call, L, {atom, L, is_list}, [{var, L, 'T'}]}]],
	      [{'case', L, {call, L, {remote, L,
				      {atom,L,lists},{atom,L,keyfind}},
			    [{var,L,'H'},{integer,L,1},{var,L,'Vals'}]},
		[{clause, L, [{atom,L,false}], [],
		  [{call, L, {var, L, 'F1'}, [{var, L, 'T'},
					      {var, L, 'R'},
					      {var, L, 'F1'}]}]},
		 {clause, L, [{tuple, L, [{var,L,'_'},{var,L,'Val'}]}],
		  [],
		  [{call, L, {var, L, 'F1'},
		    [{var, L, 'T'},
		     {call, L, {atom, L, 'setelement'},
		      [{var, L, 'Pos'}, {var, L, 'R'}, {var, L, 'Val'}]},
		     {var, L, 'F1'}]}]}
		]}
	      ]}
	    ]}}},
	 {call, L, {var, L, 'F'}, [{var, L, 'AttrNames'},
				   {var, L, 'Rec'},
				   {var, L, 'F'}]}
	]}
      ]}].

field_list(Flds) ->
    erl_parse:abstract(
      lists:zip(Flds, lists:seq(2, length(Flds)+1))).



f_get_2(R, Flds, L, Acc) ->
    FName = fname(get, R, Acc),
    {_, Types} = lists:keyfind(R, 1, Acc#pass1.record_types),
    [funspec(L, FName,
	     [{[t_atom(L, A), t_record(L, R)], T}
		 || {A, T} <- Types]
	     ++ [{[t_list(L, [t_attr(L, R, Acc)]), t_record(L, R)],
		  t_list(L, [t_union(L, [Ts || {_, Ts} <- Types])])}]
	    ),
    {function, L, FName, 2,
     [{clause, L, [{var, L, 'Attrs'}, {var, L, 'R'}],
       [[{call, L, {atom, L, is_list}, [{var, L, 'Attrs'}]}]],
       [{lc, L, {call, L, {atom, L, FName}, [{var, L, 'A'}, {var, L, 'R'}]},
	 [{generate, L, {var, L, 'A'}, {var, L, 'Attrs'}}]}]
       } |
      [{clause, L, [{atom, L, Attr}, {var, L, 'R'}], [],
	[{record_field, L, {var, L, 'R'}, R, {atom, L, Attr}}]} ||
	  Attr <- Flds]] ++
     [{clause, L, [{var, L, 'Attr'}, {var, L, 'R'}], [],
       [bad_record_op(L, FName, 'Attr', 'R')]}]
    }].


f_info(Acc, L) ->
    Fname = list_to_atom(fname_prefix(info, Acc)),
    [funspec(L, Fname,
	     [{[t_atom(L, R)],
	       t_list(L, [t_union(L, [t_atom(L,A) ||
					 A <- get_flds(R, Acc)])])}
	      || R <- Acc#pass1.exports]),
     {function, L, Fname, 1,
      [{clause, L,
	[{var, L, 'RecName'}], [],
	[{call, L, {atom, L, Fname}, [{var, L, 'RecName'}, {atom, L, fields}]}]
       }]}
    ].

f_isrec_2(#pass1{records = Rs, exports = Es} = Acc, L) ->
    Fname = list_to_atom(fname_prefix(is_record, Acc)),
    Info = [{R,length(As) + 1} || {R,As} <- Rs, lists:member(R, Es)],
    [funspec(L, Fname,
	     [{[t_atom(L, R), t_record(L, R)], t_atom(L, true)}
	      || R <- Es] ++
		 [{[t_any(L), t_any(L)], t_atom(L, false)}]),
     {function, L, Fname, 2,
      lists:map(
	fun({R, Ln}) ->
		{clause, L,
		 [{atom, L, R}, {var, L, 'Rec'}],
		 [[{op,L,'==',
		    {call, L, {atom,L,tuple_size},[{var,L,'Rec'}]},
		    {integer, L, Ln}},
		   {op,L,'==',
		    {call,L,{atom,L,element},[{integer,L,1},
					      {var,L,'Rec'}]},
		    {atom, L, R}}]],
		 [{atom, L, true}]}
	end, Info) ++
	  [{clause, L, [{var,L,'_'}, {var,L,'_'}], [],
	    [{atom, L, false}]}]}
    ].


f_info_2(Acc, L) ->
    Fname = list_to_atom(fname_prefix(info, Acc)),
    [funspec(L, Fname,
	     lists:flatmap(
	       fun(Rname) ->
		       Flds = get_flds(Rname, Acc),
		       TRec = t_atom(L, Rname),
		       [{[TRec, t_atom(L, size)], t_integer(L, length(Flds)+1)},
			{[TRec, t_atom(L, fields)],
			 t_list(L, [t_union(L, [t_atom(L, F) || F <- Flds])])}]
	       end, Acc#pass1.exports)),
     {function, L, Fname, 2,
      [{clause, L,
	[{atom, L, R},
	 {var, L, 'Info'}],
	[],
	[{call, L, {atom, L, fname(info, R, Acc)}, [{var, L, 'Info'}]}]} ||
	  R <- Acc#pass1.exports]}
    ].

f_info_3(Versions, L, Acc) ->
    Fname = list_to_atom(fname_prefix(info, Acc)),
    [
    {function, L, Fname, 3,
     [{clause, L,
       [{atom, L, R},
        {var, L, 'Info'},
        {string, L, V}],
       [],
       [{call, L, {atom, L, fname(info,R,V,Acc)}, [{var, L, 'Info'}]}]} ||
         {R,V} <- flat_versions(Versions)]}
    ].

f_pos_2(#pass1{exports = Es} = Acc, L) ->
    Fname = list_to_atom(fname_prefix(pos, Acc)),
    [
     funspec(L, Fname, lists:flatmap(
			 fun(R) ->
				 Flds = get_flds(R, Acc),
				 PFlds = lists:zip(
					   lists:seq(1, length(Flds)), Flds),
				 [{[t_atom(L, R), t_atom(L, A)],
				   t_integer(L, P)} || {P,A} <- PFlds]
			 end, Es)),
     {function, L, Fname, 2,
      [{clause, L,
	[{atom, L, R},
	 {var, L, 'Attr'}],
	[],
	[{call, L, {atom, L, fname(pos, R, Acc)}, [{var, L, 'Attr'}]}]} ||
	  R <- Acc#pass1.exports]}
    ].

f_isrec_1(Acc, L) ->
    Fname = list_to_atom(fname_prefix(is_record, Acc)),
    [funspec(L, Fname,
	     [{[t_record(L, R)], t_atom(L, true)}
	      || R <- Acc#pass1.exports]
	     ++ [{[t_any(L)], t_atom(L, false)}]),
     {function, L, Fname, 1,
      [{clause, L,
	[{var, L, 'X'}],
	[],
	[{'if',L,
	  [{clause, L, [], [[{call, L, {atom,L,is_record},
			      [{var,L,'X'},{atom,L,R}]}]],
	    [{atom,L,true}]} || R <- Acc#pass1.exports] ++
	      [{clause,L, [], [[{atom,L,true}]],
		[{atom, L, false}]}]}]}
      ]}
    ].



f_get(#pass1{record_types = RTypes, exports = Es} = Acc, L) ->
    Fname = list_to_atom(fname_prefix(get, Acc)),
    [funspec(L, Fname,
	     lists:append(
	       [[{[t_atom(L, A), t_record(L, R)], T}
		 || {A, T} <- Types]
		|| {R, Types} <- RTypes, lists:member(R, Es)])
	     ++ [{[t_list(L, [t_attr(L, R, Acc)]), t_record(L, R)],
		  t_list(L, [t_union(L, [Ts || {_, Ts} <- Types])])}
		 || {R, Types} <- RTypes, lists:member(R, Es)]
	    ),
     {function, L, Fname, 2,
      [{clause, L,
	[{var, L, 'Attrs'},
	 {var, L, 'Rec'}],
	[[{call, L,
	   {atom, L, is_record},
	   [{var, L, 'Rec'}, {atom, L, R}]}]],
	[{call, L, {atom, L, fname(get, R, Acc)}, [{var, L, 'Attrs'},
						   {var, L, 'Rec'}]}]} ||
	  R <- Es]}
    ].


f_set(Acc, L) ->
    Fname = list_to_atom(fname_prefix(set, Acc)),
    [funspec(L, Fname,
	     lists:map(
	       fun(Rname) ->
		       TRec = t_record(L, Rname),
		       {[t_list(L, [t_prop(L, Rname, Acc)]), TRec], TRec}
	       end, Acc#pass1.exports)),
     {function, L, Fname, 2,
      [{clause, L,
	[{var, L, 'Vals'},
	 {var, L, 'Rec'}],
	[[{call, L,
	   {atom, L, is_record},
	   [{var, L, 'Rec'}, {atom, L, R}]}]],
	[{call, L, {atom, L, fname(set, R, Acc)}, [{var, L, 'Vals'},
						   {var, L, 'Rec'}]}]} ||
	  R <- Acc#pass1.exports]}
    ].

f_fromlist(Acc, L) ->
    Fname = list_to_atom(fname_prefix(fromlist, Acc)),
    [funspec(L, Fname,
	     lists:map(
	       fun(Rname) ->
		       TRec = t_record(L, Rname),
		       {[t_list(L, [t_prop(L, Rname, Acc)]), TRec], TRec}
	       end, Acc#pass1.exports)),
     {function, L, Fname, 2,
      [{clause, L,
	[{var, L, 'Vals'},
	 {var, L, 'Rec'}],
	[[{call, L,
	   {atom, L, is_record},
	   [{var, L, 'Rec'}, {atom, L, R}]}]],
	[{call, L, {atom, L, fname(fromlist, R, Acc)}, [{var, L, 'Vals'},
							{var, L, 'Rec'}]}]} ||
	  R <- Acc#pass1.exports]}
    ].

f_info_1(Rname, Acc, L) ->
    Fname = fname(info, Rname, Acc),
    Flds = get_flds(Rname, Acc),
    [funspec(L, Fname, [{[t_atom(L, fields)],
			 t_list(L, [t_union(L, [t_atom(L,F) || F <- Flds])])},
			{[t_atom(L, size)], t_integer(L, length(Flds))}]),
     {function, L, Fname, 1,
      [{clause, L, [{atom, L, fields}], [],
	[{call, L, {atom, L, record_info},
	  [{atom, L, fields}, {atom, L, Rname}]}]
       },
       {clause, L, [{atom, L, size}], [],
	[{call, L, {atom, L, record_info},
	  [{atom, L, size}, {atom, L, Rname}]}]
       }]}
    ].

f_info_1(Rname, Acc, L, V) ->
    f_info_1(recname(Rname, V), Acc, L).

recname(Rname, V) ->
    list_to_atom(lists:concat([Rname,"__",V])).

f_convert(_Vsns, L, Acc) ->
    {function, L, fname(convert, Acc), 2,
     [{clause, L,
       [{var, L, 'FromVsn'},
        {var, L, 'Rec'}],
       [[{call,L,{atom, L, is_tuple},
         [{var, L, 'Rec'}]}]],
       [{match, L, {var, L, 'Rname'},
         {call, L, {atom, L, element},
          [{integer, L, 1}, {var, 1, 'Rec'}]}},
        {match,L,{var,L,'Size'},
         {call, L, {atom, L, fname(info, Acc)},
          [{var,L,'Rname'}, {atom, L, size}, {var,L,'FromVsn'}]}},
        {match, L, {var, L, 'Size'},
         {call, L, {atom, L, size},
          [{var, L, 'Rec'}]}},
        %%
        %% {match, L, {var, L, 'Old_fields'},
        %%  {call, L, {atom,L,fname(info, Acc)},
        %%     [{var,L,'Rname'},{atom,L,fields},{var,L,'FromVsn'}]}},
        {match, L, {var, L, 'New_fields'},
         {call, L, {atom,L,fname(info, Acc)},
            [{var,L,'Rname'},{atom,L,fields}]}},

        {match, L, {var, L, 'Values'},
         {call, L, {remote, L, {atom, L, lists}, {atom, L, zip}},
          [{call, L, {atom,L,fname(info, Acc)},
            [{var,L,'Rname'},{atom,L,fields},{var,L,'FromVsn'}]},
           {call, L, {atom, L, 'tl'},
            [{call, L, {atom, L, tuple_to_list},
              [{var, L, 'Rec'}]}]}]}},
        {match, L, {tuple, L, [{var, L, 'Matching'},
                               {var, L, 'Discarded'}]},
         {call, L, {remote, L, {atom, L, lists}, {atom, L, partition}},
          [{'fun',L,
            {clauses,
             [{clause,L,
               [{tuple,L,[{var,L,'F'},{var,L,'_'}]}],
               [],
               [{call,L,
                 {remote,L,{atom,L,lists},{atom,L,member}},
                 [{var, L, 'F'}, {var,L,'New_fields'}]}]}]}},
           {var, L, 'Values'}]}},
        {tuple, L, [{call, L, {atom, L, fname(set, Acc)},
                     [{var, L, 'Matching'},
                      {call, L, {atom, L, fname(new, Acc)},
                       [{var, L, 'Rname'}]}]},
                    {var, L, 'Discarded'}]}]
      }]}.

%%% ========== generic parse_transform stuff ==============

-spec context(atom(), #context{}) ->
    term().
%% @hidden
context(module,   #context{module = M}  ) -> M;
context(function, #context{function = F}) -> F;
context(arity,    #context{arity = A}   ) -> A.



rpt_error(Reason, Fun, Info) ->
    Fmt = lists:flatten(
	    ["*** ERROR in parse_transform function:~n"
	     "*** Reason     = ~p~n",
             "*** Location: ~p~n",
	     ["*** ~10w = ~p~n" || _ <- Info]]),
    Args = [Reason, Fun |
	    lists:foldr(
	      fun({K,V}, Acc) ->
		      [K, V | Acc]
	      end, [], Info)],
    io:format(Fmt, Args).

-spec format_error({atom(), term()}) ->
    iolist().
%% @hidden
format_error({_Cat, Error}) ->
    Error.
