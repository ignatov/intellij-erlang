-module(test).
%% NOTE: src_parse_transform is located in src/ so it is not reflected in dependencies of test sources.
-compile([{parse_transform, test_parse_transform}, {parse_transform, src_parse_transform}]).