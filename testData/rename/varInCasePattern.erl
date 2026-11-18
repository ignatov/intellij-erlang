-module(varInCasePattern).
-export([foo/1]).

foo(A<caret>) ->
  case x of
    A ->
      A
  end.
