-export([foo/0]).

foo() -> spawn(fun foo/0).