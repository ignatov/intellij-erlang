-module(nestedIfElse).

-ifndef(x).

  foo() -> ok.

  -ifdef(y).
    commented_out() -> ok.
  -else.
    not_commented_out() -> ok.
  -endif.

-endif.