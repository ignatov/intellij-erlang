-module(quotedAtom).

-export_type(['my-type'/0]).

-type 'my-<caret>type'() :: atom().
