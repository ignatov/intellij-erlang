%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(dialyzer_cl_parse).

-export([start/0, get_lib_dir/1]).
-export([collect_args/1]).	% used also by typer

-include("dialyzer.hrl").

%%-----------------------------------------------------------------------

-type dial_cl_parse_ret() :: {'check_init', #options{}}
                           | {'plt_info', #options{}}
                           | {'cl', #options{}}
                           | {{'gui', 'gs' | 'wx'}, #options{}}
                           | {'error', string()}.

-type deep_string() :: string() | [deep_string()].

%%-----------------------------------------------------------------------

-spec start() -> dial_cl_parse_ret().

start() ->
  init(),
  Args = init:get_plain_arguments(),
  try
    cl(Args)
  catch
    throw:{dialyzer_cl_parse_error, Msg} -> {error, Msg};
    _:R ->
      Msg = io_lib:format("~p\n~p\n", [R, erlang:get_stacktrace()]),
      {error, lists:flatten(Msg)}
  end.

cl(["--add_to_plt"|T]) ->
  put(dialyzer_options_analysis_type, plt_add),
  cl(T);
cl(["--apps"|T]) ->
  T1 = get_lib_dir(T),
  {Args, T2} = collect_args(T1),
  append_var(dialyzer_options_files_rec, Args),
  cl(T2);
cl(["--build_plt"|T]) ->
  put(dialyzer_options_analysis_type, plt_build),
  cl(T);
cl(["--check_plt"|T]) ->
  put(dialyzer_options_analysis_type, plt_check),
  cl(T);
cl(["-n"|T]) ->
  cl(["--no_check_plt"|T]);
cl(["--no_check_plt"|T]) ->
  put(dialyzer_options_check_plt, false),
  cl(T);
cl(["-nn"|T]) ->
  cl(["--no_native"|T]);
cl(["--no_native"|T]) ->
  put(dialyzer_options_native, false),
  cl(T);
cl(["--plt_info"|T]) ->
  put(dialyzer_options_analysis_type, plt_info),
  cl(T);
cl(["--get_warnings"|T]) ->
  put(dialyzer_options_get_warnings, true),
  cl(T);
cl(["-D"|_]) ->
  cl_error("No defines specified after -D");
cl(["-D"++Define|T]) ->
  Def = re:split(Define, "=", [{return, list}]),
  append_defines(Def),
  cl(T);
cl(["-h"|_]) ->
  help_message();
cl(["--help"|_]) ->
  help_message();
cl(["-I"]) ->
  cl_error("no include directory specified after -I");
cl(["-I", Dir|T]) ->
  append_include(Dir),
  cl(T);
cl(["-I"++Dir|T]) ->
  append_include(Dir),
  cl(T);
cl(["-c"++_|T]) ->
  NewTail = command_line(T),
  cl(NewTail);
cl(["-r"++_|T0]) ->
  {Args, T} = collect_args(T0),
  append_var(dialyzer_options_files_rec, Args),
  cl(T);
cl(["--remove_from_plt"|T]) ->
  put(dialyzer_options_analysis_type, plt_remove),
  cl(T);
cl(["--com"++_|T]) ->
  NewTail = command_line(T),
  cl(NewTail);
cl(["--output"]) ->
  cl_error("No outfile specified");
cl(["-o"]) ->
  cl_error("No outfile specified");
cl(["--output",Output|T]) ->
  put(dialyzer_output, Output),
  cl(T);
cl(["--output_plt"]) ->
  cl_error("No outfile specified for --output_plt");
cl(["--output_plt",Output|T]) ->
  put(dialyzer_output_plt, Output),
  cl(T);
cl(["-o", Output|T]) ->
  put(dialyzer_output, Output),
  cl(T);
cl(["-o"++Output|T]) ->
  put(dialyzer_output, Output),
  cl(T);
cl(["--raw"|T]) ->
  put(dialyzer_output_format, raw),
  cl(T);
cl(["--fullpath"|T]) ->
  put(dialyzer_filename_opt, fullpath),
  cl(T);
cl(["-pa", Path|T]) ->
  case code:add_patha(Path) of
    true -> cl(T);
    {error, _} -> cl_error("Bad directory for -pa: " ++ Path)
  end;
cl(["--plt"]) ->
  error("No plt specified for --plt");
cl(["--plt", PLT|T]) ->
  put(dialyzer_init_plts, [PLT]),
  cl(T);
cl(["--plts"]) ->
  error("No plts specified for --plts");
cl(["--plts"|T]) ->
  {PLTs, NewT} = get_plts(T, []),
  put(dialyzer_init_plts, PLTs),
  cl(NewT);
cl(["-q"|T]) ->
  put(dialyzer_options_report_mode, quiet),
  cl(T);
cl(["--quiet"|T]) ->
  put(dialyzer_options_report_mode, quiet),
  cl(T);
cl(["--src"|T]) ->
  put(dialyzer_options_from, src_code),
  cl(T);
cl(["--no_spec"|T]) ->
  put(dialyzer_options_use_contracts, false),
  cl(T);
cl(["-v"|_]) ->
  io:format("Dialyzer version "++?VSN++"\n"),
  erlang:halt(?RET_NOTHING_SUSPICIOUS);
cl(["--version"|_]) ->
  io:format("Dialyzer version "++?VSN++"\n"),
  erlang:halt(?RET_NOTHING_SUSPICIOUS);
cl(["--verbose"|T]) ->
  put(dialyzer_options_report_mode, verbose),
  cl(T);
cl(["-W"|_]) ->
  cl_error("-W given without warning");
cl(["-Whelp"|_]) ->
  help_warnings();
cl(["-W"++Warn|T]) ->
  append_var(dialyzer_warnings, [list_to_atom(Warn)]),
  cl(T);
cl(["--dump_callgraph"]) ->
  cl_error("No outfile specified for --dump_callgraph");
cl(["--dump_callgraph", File|T]) ->
  put(dialyzer_callgraph_file, File),
  cl(T);
cl(["--gui"|T]) ->
  put(dialyzer_options_mode, {gui, gs}),
  cl(T);
cl(["--wx"|T]) ->
  put(dialyzer_options_mode, {gui, wx}),
  cl(T);
cl([H|_] = L) ->
  case filelib:is_file(H) orelse filelib:is_dir(H) of
    true ->
      NewTail = command_line(L),
      cl(NewTail);
    false ->
      cl_error("Unknown option: " ++ H)
  end;
cl([]) ->
  {RetTag, Opts} =
    case get(dialyzer_options_analysis_type) =:= plt_info of
      true ->
	put(dialyzer_options_analysis_type, plt_check),
	{plt_info, cl_options()};
      false ->
	case get(dialyzer_options_mode) of
	  {gui, _} = GUI -> {GUI, common_options()};
	  cl ->
	    case get(dialyzer_options_analysis_type) =:= plt_check of
	      true  -> {check_init, cl_options()};
	      false -> {cl, cl_options()}
	    end
	end
    end,
  case dialyzer_options:build(Opts) of
    {error, Msg} -> cl_error(Msg);
    OptsRecord -> {RetTag, OptsRecord}
  end.

%%-----------------------------------------------------------------------

command_line(T0) ->
  {Args, T} = collect_args(T0),
  append_var(dialyzer_options_files, Args),
  %% if all files specified are ".erl" files, set the 'src' flag automatically
  case lists:all(fun(F) -> filename:extension(F) =:= ".erl" end, Args) of
    true -> put(dialyzer_options_from, src_code);
    false -> ok
  end,
  T.

-spec cl_error(deep_string()) -> no_return().

cl_error(Str) ->
  Msg = lists:flatten(Str),
  throw({dialyzer_cl_parse_error, Msg}).

init() ->
  put(dialyzer_options_mode, cl),
  put(dialyzer_options_files_rec, []),
  put(dialyzer_options_report_mode, normal),
  put(dialyzer_warnings, []),
  DefaultOpts = #options{},
  put(dialyzer_include,           DefaultOpts#options.include_dirs),
  put(dialyzer_options_defines,   DefaultOpts#options.defines),
  put(dialyzer_options_files,     DefaultOpts#options.files),
  put(dialyzer_output_format,     formatted),
  put(dialyzer_filename_opt,      basename),
  put(dialyzer_options_check_plt, DefaultOpts#options.check_plt),
  ok.

append_defines([Def, Val]) ->
  {ok, Tokens, _} = erl_scan:string(Val++"."),
  {ok, ErlVal} = erl_parse:parse_term(Tokens),
  append_var(dialyzer_options_defines, [{list_to_atom(Def), ErlVal}]);
append_defines([Def]) ->
  append_var(dialyzer_options_defines, [{list_to_atom(Def), true}]).

append_include(Dir) ->
  append_var(dialyzer_include, [Dir]).

append_var(Var, List) when is_list(List) ->
  put(Var, get(Var) ++ List),
  ok.

%%-----------------------------------------------------------------------

-spec collect_args([string()]) -> {[string()], [string()]}.

collect_args(List) ->
  collect_args_1(List, []).

collect_args_1(["-"++_|_] = L, Acc) ->
  {lists:reverse(Acc), L};
collect_args_1([Arg|T], Acc) ->
  collect_args_1(T, [Arg|Acc]);
collect_args_1([], Acc) ->
  {lists:reverse(Acc), []}.

%%-----------------------------------------------------------------------

cl_options() ->
  [{files, get(dialyzer_options_files)},
   {files_rec, get(dialyzer_options_files_rec)},
   {output_file, get(dialyzer_output)},
   {output_format, get(dialyzer_output_format)},
   {filename_opt, get(dialyzer_filename_opt)},
   {analysis_type, get(dialyzer_options_analysis_type)},
   {get_warnings, get(dialyzer_options_get_warnings)},
   {callgraph_file, get(dialyzer_callgraph_file)}
   |common_options()].

common_options() ->
  [{defines, get(dialyzer_options_defines)},
   {from, get(dialyzer_options_from)},
   {include_dirs, get(dialyzer_include)},
   {plts, get(dialyzer_init_plts)},
   {output_plt, get(dialyzer_output_plt)},
   {report_mode, get(dialyzer_options_report_mode)},
   {use_spec, get(dialyzer_options_use_contracts)},
   {warnings, get(dialyzer_warnings)},
   {check_plt, get(dialyzer_options_check_plt)}].

%%-----------------------------------------------------------------------

-spec get_lib_dir([string()]) -> [string()].

get_lib_dir(Apps) ->
  get_lib_dir(Apps, []).

get_lib_dir([H|T], Acc) ->
  NewElem =
    case code:lib_dir(list_to_atom(H)) of
      {error, bad_name} ->
	case H =:= "erts" of % hack for including erts in an un-installed system
	  true -> filename:join(code:root_dir(), "erts/preloaded/ebin");
	  false -> H
	end;
      LibDir -> LibDir ++ "/ebin"
    end,
  get_lib_dir(T, [NewElem|Acc]);
get_lib_dir([], Acc) ->
  lists:reverse(Acc).

%%-----------------------------------------------------------------------

get_plts(["--"|T], Acc) -> {lists:reverse(Acc), T};
get_plts(["-"++_Opt = H|T], Acc) -> {lists:reverse(Acc), [H|T]};
get_plts([H|T], Acc) -> get_plts(T, [H|Acc]);
get_plts([], Acc) -> {lists:reverse(Acc), []}.

%%-----------------------------------------------------------------------

-spec help_warnings() -> no_return().

help_warnings() ->
  S = warning_options_msg(),
  io:put_chars(S),
  erlang:halt(?RET_NOTHING_SUSPICIOUS).

-spec help_message() -> no_return().

help_message() ->
  S = "Usage: dialyzer [--help] [--version] [--shell] [--quiet] [--verbose]".