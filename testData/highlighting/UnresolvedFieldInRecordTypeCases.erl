-export_type([t/0, u/0, v/0, w/0]).
-export([f/1]).

-define(FIELDS, field1, field2 = default_value).
-define(F, a).
-record(r, {a, b}).
-record(m, {?FIELDS, field3}).

-type t() :: #<error>unknown</error>{c :: integer()}.
-type u() :: #r{?F :: integer(), b :: atom()}.
-type v() :: #m{field1 :: integer(), field2 :: atom(), field3 :: atom(), <error>field4</error> :: atom()}.
-type w() :: [#r{a :: integer(), <error>z</error> :: integer()}].

-spec f(#r{<error>d</error> :: integer()}) -> ok.
f(_) -> ok.
