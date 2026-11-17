-module(native_records_resolve).
-export([local/0, imported/0, qualified/0, bad_field/0, not_exported/0, missing/0]).

-record #local{x = 0}.
-export_record([local]).
-import_record(native_records_remote, [point]).

local() ->
  #local{x = 1}.

imported() ->
  #point{x = 1, y = 2}.

qualified() ->
  #native_records_remote:point{x = 1}.

bad_field() ->
  #point{x = 1, <error>unknown</error> = 2}.

not_exported() ->
  #<error>native_records_remote:hidden</error>{secret = 1}.

missing() ->
  #<error>missing</error>{}.
