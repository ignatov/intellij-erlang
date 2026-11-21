-export([foo/1]).

-export([foo<caret>/0_1]).

foo(_) -> ok.