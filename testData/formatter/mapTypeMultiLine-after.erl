-module(fmt_map).
-export_type([opts/0, one/0]).

-type opts() :: #{
  length => integer(),
  period => integer()
}.
-type one() :: #{
  a => b}.
