-module(shop_typed).

-export([label/1]).

%% Compiles, but eqWAlizer reports that a string is not a binary.
-spec label(atom()) -> binary().
label(Name) ->
  atom_to_list(Name).
