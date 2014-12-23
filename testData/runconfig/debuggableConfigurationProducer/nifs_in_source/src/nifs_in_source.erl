-module(nifs_in_source).

-export([nifs_in_source/0]).

nifs_in_source() -> erlang:load_nif(nifs_in_source, 0).