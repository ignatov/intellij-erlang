-export([foo/0]).

dt_get_tag() -> ok.
foo() -> dt_get_tag<caret>().