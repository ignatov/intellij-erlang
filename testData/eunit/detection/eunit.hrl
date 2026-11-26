-ifndef(EUNIT_HRL).
-define(EUNIT_HRL, true).

-ifndef(NOTEST).
-ifndef(TEST).
-define(TEST, true).
-endif.
-ifndef(EUNIT).
-define(EUNIT, true).
-endif.
-else.
-undef(EUNIT).
-endif.

-ifndef(EUNIT_NOAUTO).
-ifndef(NOTEST).
-compile({parse_transform, eunit_autoexport}).
-else.
-compile({parse_transform, eunit_striptests}).
-endif.
-endif.

-define(assert(BoolExpr), ok).

-endif.
