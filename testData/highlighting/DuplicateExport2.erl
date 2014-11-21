-export([foo/0, bar/0]).
-export([<warning>bar/0</warning>]).
-export([tar/0, <warning>bar/0</warning>, <warning>foo/0</warning>]).

foo() -> ok.
bar() -> ok.
tar() -> ok.