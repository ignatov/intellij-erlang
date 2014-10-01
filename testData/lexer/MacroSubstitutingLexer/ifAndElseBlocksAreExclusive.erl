-module(ifAndElseBlocksAreExclusive).

-define(x, 1).

-ifdef(x).
  this_form_should_not_be_commented_out() -> ?x.
-else.
  this_form_should_be_commented_out() -> ?x.
-endif.

-ifdef(x).
  this_form_should_not_be_commented_out() -> ?x.
-else
  this_form_should_be_commented_out() -> ?x.
-endif.

-ifndef(x).
  this_form_should_be_commented_out() -> ?x.
-else.
  this_form_should_not_be_commented_out() -> ?x.
-endif.

-ifndef(x).
  this_form_should_be_commented_out() -> ?x.
-else
  this_form_should_not_be_commented_out() -> ?x.
-endif.