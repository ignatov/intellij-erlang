-module(macrosInAttrs).

-define(BAR, 11).

-ifdef(BAR).
-undef(BAR).
-endif.

-ifndef(<caret>BAR).
-endif.