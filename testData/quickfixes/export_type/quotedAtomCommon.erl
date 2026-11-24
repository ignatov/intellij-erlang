-module(quotedAtomCommon).

-export_type([bar/0]).

-type bar() :: atom().
-type 'my-<caret>type'() :: atom().
