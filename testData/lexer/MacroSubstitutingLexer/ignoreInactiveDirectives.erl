-module(ignoreInactiveDirectives).

-define(defined_macro, 20).

-ifdef(undefined_macro).
  -define(should_not_be_defined, 10).
  -undef(defined_macro).
-endif.

-ifdef(should_not_be_defined).
  foo() -> ?should_not_be_defined.
-else.
  foo() -> ?should_not_be_defined.
-endif.

-ifdef(defined_macro).
  not_commented_out() -> ?defined_macro.
-endif.