-module(m).
-behaviour(b).
-export(['my-cb'/0, 'init'/1, plain/0]).

'my-cb'() -> ok.

'init'(_) -> ok.

plain() -> ok.
