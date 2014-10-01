-module(freeMacroIsUndefined).

-ifndef(x).
  this_form_should_not_be_commented_out() -> ?x.
-endif.

-ifdef(x).
  this_form_should_be_commented_out() -> ?x.
-endif.