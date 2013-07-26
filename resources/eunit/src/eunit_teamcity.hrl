%% represents a tests group (suite)
-record(group, {name = "", location, depth = 0}).

%%
%% 'groups' are normal groups. Group events should be issued on their addition and removal.
%% 'pseudogroups' are same as normal groups except for they are to be ignored and no events should be issued.
-record(state, {groups = [], pseudogroups = []}).