-module(macrosInAttrs).

-define(FOO, 11).

-ifdef(FOO).
-undef(FOO).
-endif.

-ifndef(<caret>FOO).
-endif.