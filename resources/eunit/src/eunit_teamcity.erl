-module(eunit_teamcity).
-behaviour(eunit_listener).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_teamcity.hrl").

-export([start/0, start/1]).

-export([init/1, handle_begin/3, handle_end/3, handle_cancel/3,
         terminate/2]).

%%
%% eunit_listener API impl
%%

start() ->
    start([]).

start(Options) ->
%% noinspection ErlangUnresolvedFunction
    eunit_listener:start(?MODULE, Options).

init(_Options) ->
    receive
        {start, _Reference} ->
            #state{}
    end.

terminate({ok, _Data}, _St) ->
    sync_end(ok);
terminate({error, Reason}, _St) ->
    io:format("~nTest runner terminated. Reason: ~p~n", [Reason]),
    sync_end(error).

sync_end(Result) ->
    receive
        {stop, Reference, ReplyTo} ->
            ReplyTo ! {result, Reference, Result},
            ok
    end.

handle_begin(group, Data, State) ->
    begin_group(Data, State);
handle_begin(test, Data, State) ->
    begin_test(Data, State).

handle_end(group, Data, State) ->
    end_group(Data, State);
handle_end(test, Data, State) ->
    end_test(Data, State).

handle_cancel(group, Data, State) ->
    cancel_group(Data, State);
handle_cancel(test, Data, State) ->
    cancel_test(Data, State).


%%
%% group messages handling
%%

begin_group(Data, State) ->
    begin_group(create_group(Data), Data, State).

end_group(Data, State) ->
    end_group(create_group(Data), Data, State).

cancel_group(Data, State) ->
    State1 = case proplists:get_value(reason, Data) of
        Reason ->
            io:format("Test suite was cancelled. Reason: ~p~n", [Reason]),
            State
    end,
    Group = create_group(Data),
    end_group(Group, Data, State1).

end_group(Group, Data, State) ->
    State1 = end_child_groups(Group, Data, State),
    Attributes = case get_group_kind(Group) of
                     normal_group -> normal_group_attributes(Group);
                     name_only_group -> name_only_group_attributes(Group);
                     _ -> []
                 end,
    do_end_group(is_pseudo_group(Group, State1), Group, Attributes, Data, State1).

do_end_group(false = _IsPseudoGroup, _Group, Attributes, _Data, State) ->
    print_teamcity_message(end_group_attributes(Attributes)),
    remove_group(State);
do_end_group(true = _IsPseudoGroup, _Group, _Attributes, _Data, State) ->
    remove_pseudo_group(State).

end_child_groups(Group, Data, State) ->
    case get_current_group(State) of
        #group{depth = Depth} = LastGroup ->
            if
                Group#group.depth < Depth ->
                    end_child_groups(Group, Data, end_group(LastGroup, Data, State));
                true ->
                    State
            end;
        _ ->
            State
    end.

begin_group(Group, Data, State) ->
    Attributes = case get_group_kind(Group) of
                     normal_group -> normal_group_attributes(Group);
                     name_only_group -> name_only_group_attributes(Group);
                     _ -> []
                 end,
    IsPseudoGroup = is_pseudo_group(Group, State),
    do_begin_group(IsPseudoGroup, Group, Attributes, Data, State).

do_begin_group(false = _IsPseudoGroup, Group, Attributes, Data, State) ->
    State1 = enter_new_test_item(Data, State),
    print_teamcity_message(begin_group_attributes(Attributes)),
    add_group(Group, State1);
do_begin_group(true = _IsPseudoGroup, Group, _Attributes, _Data, State) ->
    add_pseudo_group(Group, State).

add_pseudo_group(Group, #state{pseudogroups = PseudoGroups} = State) ->
    State#state{pseudogroups = [Group | PseudoGroups]}.

add_group(Group, #state{groups = GroupsStack} = State) ->
    State#state{groups = [Group|GroupsStack]}.

remove_pseudo_group(#state{pseudogroups = []} = State) -> State;
remove_pseudo_group(#state{pseudogroups = [_Group | Groups]} = State) ->
    State#state{pseudogroups = Groups}.

remove_group(#state{groups = []} = State) -> State;
remove_group(#state{groups = [_Group|Groups]} = State) ->
    State#state{groups = Groups}.

end_group_attributes(AdditionalAttributes) ->
    ["testSuiteFinished" | AdditionalAttributes].

begin_group_attributes(AdditionalAttributes) ->
    ["testSuiteStarted" | AdditionalAttributes].

normal_group_attributes(Group) ->
    NameAttribute = name_attribute(Group#group.name),
    LocationAttribute = location_attribute(Group#group.location),
    [NameAttribute, LocationAttribute].

name_only_group_attributes(Group) ->
    [name_attribute(Group#group.name)].

create_group(Data) ->
    Name = get_group_name(Data),
    Location = get_location(Data),
    Depth = get_test_item_depth(Data),
    #group{name = Name, location = Location, depth = Depth}.

get_group_name(Data) ->
    Desc = proplists:get_value(desc, Data),
    DescModuleName = parse_module_name(Desc),
    Location = get_location(Data),
    if
        DescModuleName =/= undefined ->
            atom_to_list(DescModuleName);
        Desc =/= undefined ->
            binary_to_list(Desc);
        Location =/= undefined ->
            Module = get_module(Location),
            atom_to_list(Module);
        true ->
            undefined
    end.

get_group_kind(#group{depth = 0}) ->
    root_group;
get_group_kind(#group{name = undefined, location = undefined}) ->
    empty_group;
get_group_kind(#group{location = undefined}) ->
    name_only_group;
get_group_kind(_Group) ->
    normal_group.

%%
%% test messages handling
%%
begin_test(Data, State) ->
    TestLocation = get_location(Data),
    case is_in_current_group(TestLocation, State) of
        true ->
            do_begin_test(Data, State),
            State;
        _ ->
            State1 = enter_new_test_item(Data, State),
            Group = create_group_from_function(Data),
            State2 = begin_group(Group, Data, State1),
            do_begin_test(Data, State2)
    end.

end_test(Data, State) ->
    case proplists:get_value(status, Data) of
        {error, _} ->
            print_teamcity_message(["testFailed" | error_test_attributes(Data)]);
        _ -> ok
    end,
    print_teamcity_message(["testFinished" | normal_test_attributes(Data)]),
    State.

cancel_test(Data, State) ->
    CancellationInfo = create_cancel_test_info(Data, State),
    end_test([CancellationInfo | Data], State).

create_group_from_function(Data) ->
    FunctionLocation = get_location(Data),
    GroupLocation = get_module(FunctionLocation),
    GroupName = atom_to_list(GroupLocation),
    GroupDepth = get_test_item_depth(Data),
    #group{name = GroupName, location = GroupLocation, depth = GroupDepth}.

create_cancel_test_info(Data, _State) ->
    Reason = proplists:get_value(reason, Data),
    StatusMessage = lists:flatten(io_lib:format("The test was cancelled. Reason: ~w", [Reason])),
    {status, StatusMessage}.

do_begin_test(Data, State) ->
    print_teamcity_message(["testStarted" | normal_test_attributes(Data)]),
    State.

error_test_attributes(Data) ->
    Message = message_attribute(get_test_failed_message(Data)),
    Details = details_attribute(get_test_failed_details(Data)),
    normal_test_attributes(Data) ++ [Message, Details].

normal_test_attributes(Data) ->
    Location = get_location(Data),
    LocationAttribute = location_attribute(Location),
    NameAttribute = name_attribute(get_test_name(Data)),
    [NameAttribute, LocationAttribute].

get_test_failed_details(Data) ->
    case proplists:get_value(output, Data) of
        undefined ->
            undefined;
        <<>> ->
            undefined;
        Output ->
            binary_to_list(Output)
    end.

get_test_failed_message(Data) ->
    case proplists:get_value(status, Data) of
        {error, Exception} ->
            lists:flatten(eunit_lib:format_exception(Exception));
        undefined -> undefined;
        StatusInfo -> lists:flatten(io_lib:format("Test failed. ~w", [StatusInfo]))
    end.

is_in_current_group(TestLocation, State) ->
    case first_normal_group(State) of
        #group{location = GroupLocation} ->
            GroupLocation == get_module(TestLocation);
        _ ->
            false
    end.

get_test_name(Data) ->
    Desc = proplists:get_value(desc, Data),
    DescModuleName = parse_module_name(Desc),
    if
        DescModuleName =:= undefined andalso Desc =/= undefined ->
            binary_to_list(Desc);
        true ->
            Location = get_location(Data),
            atom_to_list(get_function(Location))
    end.

%%
%% utils
%%

is_pseudo_group(Group, #state{pseudogroups = [Group|_]}) -> true;
is_pseudo_group(Group, State) ->
    case get_group_kind(Group) of
        empty_group -> true;
        root_group -> true;
        _ ->
            case first_normal_group(State) of
                #group{name = Name} ->
                    (Name ++ "_tests")
                    ==
                    Group#group.name;
                _ ->
                    false
            end
    end.

get_current_group(#state{groups = [], pseudogroups = []})        -> undefined;
get_current_group(#state{groups = [Group|_], pseudogroups = []}) -> Group;
get_current_group(#state{groups = [], pseudogroups = [Group|_]}) -> Group;
get_current_group(#state{groups = [G1|_], pseudogroups = [G2|_]}) ->
    if
        G1#group.depth >= G2#group.depth ->
            G1;
        true ->
            G2
    end.

first_normal_group(#state{groups = Groups, pseudogroups = PseudoGroups}) ->
    first_normal_group(Groups, PseudoGroups).

first_normal_group([], []) -> undefined;
first_normal_group([Group | Groups], []) ->
    first_normal_group(Group, Groups, []);
first_normal_group([], Groups) ->
    first_normal_group(Groups, []);
first_normal_group([#group{depth = G1Depth} = G1 | Groups1Tail] = Groups1,
                   [#group{depth = G2Depth} = G2 | Groups2Tail] = Groups2) ->
    if
        G1Depth >= G2Depth ->
            first_normal_group(G1, Groups1Tail, Groups2);
        true ->
            first_normal_group(G2, Groups1, Groups2Tail)
    end.

first_normal_group(Group, Groups1, Groups2) ->
    case get_group_kind(Group) of
        normal_group ->
            Group;
        _ -> first_normal_group(Groups1, Groups2)
    end.

get_test_item_depth(Data) ->
    case proplists:get_value(id, Data) of
        List when is_list(List) -> length(List);
        _ -> 0
    end.

%% closes current group if a new test item is on the same depth
enter_new_test_item(Data, State) ->
    Depth = get_test_item_depth(Data),
    case get_current_group(State) of
        #group{depth = Depth} = CurrentGroup ->
            end_group(CurrentGroup, Data, State);
        _ -> State
    end.

get_location(Data) ->
    case proplists:get_value(source, Data) of
        {Module, Function, _Arity} ->
            case proplists:get_value(line, Data) of
                Line when is_integer(Line) ->
                    {Module, Function, Line};
                _ -> {Module, Function, -1}
            end;
        undefined ->
            parse_module_name(proplists:get_value(desc, Data))
    end.

get_module({Module, _Function, _Line})  -> Module;
get_module(Module) when is_atom(Module) -> Module;
get_module(_)                           -> undefined.

get_function({_Module, Function, _Line}) -> Function;
get_function(_)                          -> undefined.

parse_module_name(undefined) -> undefined;
parse_module_name(DescBinaryString) ->
    DescString = binary_to_list(DescBinaryString),
    case re:run(DescString, "^module '([^']+)'$") of
        nomatch ->
            undefined;
        {match, [_, {FirstIdx, LastIdx}]} ->
            ModuleName = string:sub_string(DescString, FirstIdx + 1, FirstIdx + LastIdx),
            list_to_atom(ModuleName)
    end.

print_teamcity_message(Attributes) ->
    Message = string:join(Attributes, " "),
    io:format("~s~n", ["##teamcity[" ++ Message ++ "]"]).

message_attribute(Message) ->
    attribute_str("message", Message).

details_attribute(Details) ->
    attribute_str("details", Details).

name_attribute(Name) ->
    attribute_str("name", Name).

location_attribute(undefined) -> "";
location_attribute(Location) ->
    attribute_str("locationHint", "eunit://" ++ location_str(Location)).
location_str({Module, Function, Line}) ->
    M = atom_to_list(Module),
    F = atom_to_list(Function),
    L = lists:flatten(io_lib:format("~p", [Line])),
    M ++ ":" ++ F ++ ":" ++ L;
location_str(Module) when is_atom(Module) ->
    atom_to_list(Module).

attribute_str(_AttributeName, undefined) -> "";
attribute_str(AttributeName, AttributeValue) ->
    AttributeName ++ "=" ++ "'" ++ escape_chars(AttributeValue) ++ "'".

escape_chars(BinaryStr) when is_binary(BinaryStr) ->
    escape_chars(binary_to_list(BinaryStr));
escape_chars(Str) ->
    lists:flatten(lists:map(fun escape_map/1, Str)).

escape_map($')  -> "|'";
escape_map($\n) -> "|n";
escape_map($\r) -> "|r";
escape_map($|)  -> "||";
escape_map($])  -> "|]";
escape_map(X)   -> X.