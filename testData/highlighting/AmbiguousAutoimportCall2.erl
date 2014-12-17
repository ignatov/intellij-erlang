-export([foo/0, abs/1]).

abs(I) -> I.
foo() -> erlang:abs(3).