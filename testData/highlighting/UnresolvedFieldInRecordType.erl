-export_type([t/0, u/0]).

-record(r, {a, b}).

-type t() :: #r{<error>c</error> :: integer()}.
-type u() :: #r{a :: integer()}.
