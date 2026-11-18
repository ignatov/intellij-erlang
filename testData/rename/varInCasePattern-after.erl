-module(varInCasePattern).
-export([foo/1]).

foo(B<caret>) ->
  case x of
    B ->
      B
  end.
