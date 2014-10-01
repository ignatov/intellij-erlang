-module(disabledBranchIsCommentedOut).

-undef(x).
-ifdef(x).
  this_form_should_be_commented_out() -> ?x.
-endif.